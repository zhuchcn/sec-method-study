setwd(dirname(parent.frame(2)$ofile))

pkgs=c("dplyr", "reshape2", "readxl", "readr", "zeallot", "stringr")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}

## xic
file = "../../../../data-raw/20190830/20190830 HDL shotgun proteomics summary.xlsx"
xic = read_excel(file, sheet = "area", col_names = FALSE, skip = 3)
colnames(xic) = c(
    "protein", "peptide_label", "sequence", "charge", "Mod_AAs", "ms_alias",
    paste0("F-", 0:7, "-210"), paste0("F-", 0:7, "-250")
)

xic_210 = xic[,c(1:14)] %>% as.data.frame()
colnames(xic_210)[7:14] = paste0("F", 0:7)

xic_250 = xic[,c(1:6, 15:22)] %>% as.data.frame()
colnames(xic_250)[7:14] = paste0("F", 0:7)

xic = list(
    "210" = xic_210,
    "250" = xic_250
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

xic = lapply(xic, function(table){
    cbind(
        split_protein_name(table$protein),
        table[,-1]
    )
})

## Byonic outputs
files = lapply(c("210", "250"), function(density){
    list.files("../../../../data-raw/20190830/byonic shotgun proteomics 20190830", 
               pattern = glue::glue("^HDL-fraction[0-7]-{density}.raw.xlsx$"), 
               full.names = TRUE)
}) %>% `names<-`(c("210", "250"))

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

byonic = lapply(files, function(density){
    lapply(density, function(file){
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
    }) %>%
        `names<-`(paste0("F", 0:7))
}) %>%
    `names<-`(c("210", "250"))

# Parameters to filter
params = list(
    log_prob = 4.6,
    best_log_prob = 6.9,
    best_score = 400,
    exlude = c("SHRM3")
)

xic2 = lapply(names(xic), function(density){
    table = xic[[density]]
    uniprot_ids = lapply(names(byonic[[density]]), function(fraction){
        byonic[[density]][[fraction]]$proteins %>%
            filter(
                !is.na(`# of spectra`) 
                & `|Log Prob|` >= params$log_prob
                & `Best |Log Prob|` >= params$best_log_prob
                & `Best score` >= params$best_score
                & !(short_name %in% params$exclude)
                & !grepl("Trypsin", long_name)
                & !grepl("Keratin", long_name)
            ) %>%
            `[`(,"uniprot_id", drop = TRUE) %>%
            as.character()
    }) %>%
        do.call(c, .) %>% unique
    
    res = lapply(uniprot_ids, function(id){
        table2 = table %>%
            filter(uniprot_id == id)
        table2 =  table2 %>%
            mutate(
                sample_coverage = apply(
                    table2[, paste0("F", 0:7)], 1,
                    function(row) sum(!is.na(row))
                )
            ) %>%
            filter(sample_coverage == max(sample_coverage, na.rm = TRUE)) 
        table2 = table2 %>%
            mutate(mean = rowMeans(table2[,paste0("F", 0:7)], na.rm = TRUE)) %>%
            filter(mean == max(mean, na.rm = TRUE)) %>%
            select(-mean)
        return(table2)
    }) %>%
        do.call(rbind, .)
    return(res)
}) %>%
    `names<-`(c("210", "250"))

data = list(
    xic = xic2[["210"]],
    byonic = byonic[["210"]]
)

data$xic = select(data$xic, -c(F0, F1))
colnames(data$xic)[14:19] = c("LDL", "large HDL", "medium HDL", "small HDL", "pre-beta HDL", "Plasma Proteins")

data$byonic = data$byonic[3:8]
names(data$byonic) = c("LDL", "large HDL", "medium HDL", "small HDL", "pre-beta HDL", "Plasma Proteins")

save(data, file = "data.rda")
