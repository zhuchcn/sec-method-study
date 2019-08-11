pkgs=c("dplyr", "reshape2", "readxl", "readr", "zeallot", "stringr")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}

setwd(dirname(parent.frame(2)$ofile))

## xic
file = "../../../../data-raw/xic.csv"
xic = read.csv(file, header = FALSE, skip = 3)
colnames(xic) = c(
    "protein", "peptide_label", "sequence", "charge", "ms_alias",
    paste0("F", 0:8)
)

## RT
file = "../../../../data-raw/RT.csv"
RT = read.csv(file, header = FALSE, skip = 3)
colnames(RT) = c(
    "protein", "peptide_label", "sequence", "charge", "ms_alias",
    paste0("F", 0:8)
)

split_protein_name = function(names){
    fdata = lapply(names, function(name){
        name = gsub("^sp\\|", "", name)
        c(uniprot_id, name) %<-% str_split_fixed(name, "\\|", n = 2)
        c(short_name, name) %<-% str_split_fixed(name, "_", n = 2)
        c(long_name, name) %<-% str_split_fixed(name, " OS=", n = 2)
        c(species, name) %<-% str_split_fixed(name, " OX=", n = 2)
        c(OX, name) %<-% str_split_fixed(name, " GN=", n = 2)
        c(GN, name) %<-% str_split_fixed(name, " PE=", n = 2)
        c(PE, n) %<-% str_split_fixed(name, " SV=", n = 2)
        PE = as.integer(PE)
        n = as.integer(n)
        OX = as.integer(OX)
        res = data.frame(uniprot_id, short_name, long_name, species, OX, GN, PE, n)
        return(res)
    })
    fdata = do.call(rbind, fdata)
}


xic = cbind(
    split_protein_name(xic$protein),
    xic[,-1]
)

RT = cbind(
    split_protein_name(RT$protein),
    RT[,-1]
)

## Byonic outputs
files = list.files("../../../../data-raw/", pattern = "^HDL-fraction[0-8].raw.xlsx$", 
                   full.names = TRUE)

split_protein_name2 = function(names){
    fdata = lapply(names, function(name){
        name = gsub("^>Reverse ", "", name)
        name = gsub("^>sp\\|", "", name)
        c(uniprot_id, name) %<-% str_split_fixed(name, "\\|", n = 2)
        c(short_name, name) %<-% str_split_fixed(name, "_", n = 2)
        c(long_name, name) %<-% str_split_fixed(name, " OS=", n = 2)
        c(species, name) %<-% str_split_fixed(name, " GN=", n = 2)
        c(GN, name) %<-% str_split_fixed(name, " PE=", n = 2)
        c(PE, n) %<-% str_split_fixed(name, " SV=", n = 2)
        PE = as.integer(PE)
        n = as.integer(n)
        res = data.frame(uniprot_id, short_name, long_name, species, GN, PE, n)
        return(res)
    })
    fdata = do.call(rbind, fdata)
}

byonic = lapply(files, function(file){
    spectra = read_excel(file, sheet = "Spectra")
    colnames(spectra) = gsub("\n", " ", colnames(spectra))
    spectra = cbind(
        split_protein_name2(spectra$`Protein Name`),
        spectra[,colnames(spectra) != "Protein Name"]
    )
    proteins = read_excel(file, sheet = "Proteins")
    colnames(proteins) = gsub("\n", " ", colnames(proteins))
    proteins = cbind(
        split_protein_name2(proteins$Description),
        proteins[,colnames(proteins) != "Description"]
    )
    return(list(spectra = spectra, proteins = proteins))
})
names(byonic) = paste0("F", 0:8)

data = list(
    xic = xic,
    RT = RT,
    byonic = byonic
)

save(data, file = "data.rda")