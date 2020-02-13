setwd(dirname(parent.frame(2)$ofile))

pkgs=c("dplyr", "reshape2", "tibble", "stringr", "Metabase", "readxl", "zeallot")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}


# xic table ---------------------------------------------------------------

file = "../data-raw/20190830/20190830 HDL shotgun proteomics summary.xlsx"
xic = read_excel(file, sheet = "area", col_names = FALSE, skip = 3)
colnames(xic) = c(
    "protein", "peptide_label", "sequence", "charge", "Mod_AAs", "ms_alias",
    paste0("F-", 0:7, "-210"), paste0("F-", 0:7, "-250")
)

xic = xic[,c(1:14)] %>% as.data.frame()
colnames(xic)[7:14] = paste0("F", 0:7)

# split the long string of protein info into a fdata
fdata = lapply(xic$protein, function(name){
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
}) %>% 
    do.call(rbind, .)

xic = cbind(
    fdata,
    select(xic, -protein)
)


# byonic ------------------------------------------------------------------

files = list.files("../data-raw/20190830/byonic shotgun proteomics 20190830", 
                   pattern = "^HDL-fraction[0-7]-210.raw.xlsx$", 
                   full.names = TRUE)

split_protein_name = function(names){
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
        split_protein_name(spectra$`Protein Name`),
        spectra[,colnames(spectra) != "Protein Name"]
    )
    proteins = read_excel(file, sheet = "Proteins")
    colnames(proteins) = gsub("\n", " ", colnames(proteins))
    proteins = cbind(
        split_protein_name(proteins$Description),
        proteins[,colnames(proteins) != "Description"]
    )
    return(list(spectra = spectra, proteins = proteins))
}) %>%
    `names<-`(paste0("F", 0:7))


# filter proteins ---------------------------------------------------------

params = list(
    n_peptides    = 2,
    n_spectra     = 0,
    log_prob      = 4.6,
    best_log_prob = 6.9,
    best_score    = 400,
    coverage      = 0,
    n_aa          = 0,
    exclude       = c("K1C14", "K2C6B", "K2C1", "K1C10", "K2C5", "K1C9", "K22E",
                      "K2C1B", "TRY1", "TRY3", "TRY6", "SHRM3")
)

protein_ids = lapply(names(byonic), function(fraction){
    byonic[[fraction]]$proteins %>%
        filter(
            !is.na(`# of spectra`) 
            & `# of spectra`         >= params$n_spectra 
            & `# of unique peptides` >= params$n_peptides
            & `|Log Prob|`           >= params$log_prob
            & `Best |Log Prob|`      >= params$best_log_prob
            & `Best score`           >= params$best_score
            & `Coverage %`           >= params$coverage
            & `# AA's in protein`    >= params$n_aa
            & !(short_name          %in% params$exclude)
        ) %>%
        `[`(,"uniprot_id", drop = TRUE) %>%
        as.character()
}) %>% do.call(c, .) %>% unique

xic = lapply(protein_ids, function(id) {
    xic2 = xic %>%
        filter(uniprot_id == id)
    xic2 =  xic2 %>%
        mutate(
            sample_coverage = apply(
                xic2[, paste0("F", 0:7)], 1,
                function(row) sum(!is.na(row))
            )
        ) %>%
        filter(sample_coverage == max(sample_coverage, na.rm = TRUE)) 
    xic2 = xic2 %>%
        mutate(mean = rowMeans(xic2[,paste0("F", 0:7)], na.rm = TRUE)) %>%
        filter(mean == max(mean, na.rm = TRUE)) %>%
        select(-mean)
    return(xic2)
}) %>%
    do.call(rbind, .)

saveRDA(xic, file = "xic.rda")