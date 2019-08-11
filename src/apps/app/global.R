pkgs=c(
    "dplyr", "reshape2", "tibble", "Metabase", "ggplot2", "plotly", "DT",
    "shiny", "shinydashboard", "R6"
)
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}

source("views/modules/shiny_modules.R")

import::here(DataModel, .from="models/DataModel.R")

DATA = DataModel$new()
