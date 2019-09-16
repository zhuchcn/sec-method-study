pkgs=c("dplyr", "reshape2", "ggplot2", "plotly", "DT", "shiny")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}

load("data/data.rda")

fraction_names = c("LDL", "large HDL", "medium HDL", "small HDL", "pre-beta HDL", "Plasma Proteins") 