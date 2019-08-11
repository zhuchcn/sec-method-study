setwd(dirname(parent.frame(2)$ofile))

pkgs=c("dplyr", "reshape2", "tibble", "stringr", "Metabase", "readxl", "zeallot")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}

prt_data = read_excel(
    "../data-raw/20190805 HDL shotgun proteomics.xlsx",
    sheet = 1,
    range = "A1:L125"
)

fdata = lapply(prt_data$Protein, function(row){
    row = gsub("^sp\\|", "", row)
    c(uniprot_id, row) %<-% str_split_fixed(row, "\\|", n = 2)
    c(short_name, row) %<-% str_split_fixed(row, "_", n = 2)
    c(long_name, row) %<-% str_split_fixed(row, " OS=", n = 2)
    c(species, row) %<-% str_split_fixed(row, " OX=", n = 2)
    c(OX, row) %<-% str_split_fixed(row, " GN=", n = 2)
    c(GN, row) %<-% str_split_fixed(row, " PE=", n = 2)
    c(PE, n) %<-% str_split_fixed(row, " SV=", n = 2)
    res = c(uniprot_id, short_name, long_name, species, OX, GN, PE, n)
    PE = as.integer(PE)
    n = as.integer(n)
    OX = as.integer(OX)
    names(res) = c("uniprot_id", "short_name", "long_name", "species", "OX", "GN", "PE", "n")
    return(res)
})
fdata = do.call(rbind, fdata) %>%
    as.data.frame() %>%
    mutate(
        sequence = prt_data$`Peptide Sequence`,
        charge_state = prt_data$`Charge State`
    )
rownames(fdata) = fdata$short_name

edata = prt_data[,4:12] %>% as.matrix
rownames(edata) = fdata[, "short_name", drop = TRUE]
colnames(edata) = paste0("F", 0:8)

pdata = data.frame(
    row.names = colnames(edata),
    fraction = c("Exosome", "pre LDL", rep("LDL", 2), rep("HDL", 4), "Albumin"),
    subfraction = c("Exosome", "pre LDL", "lgLDL", "smLDL", "lgHDL", "mdHDL", "smHDL", "dsHDL", "Albumin")
)

prt = ProteomicsSet(
    conc_table = conc_table(edata),
    sample_table = sample_table(pdata),
    feature_data = feature_data(fdata)
)

save(prt, file = "data.rda")