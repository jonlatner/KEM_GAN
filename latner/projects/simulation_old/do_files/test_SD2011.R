# Top commands ----

# Create empty R application (no figures, data frames, packages, etc.)
# Get a list of all loaded packages
packages <- search()[grepl("package:", search())]
# Unload each package
for (package in packages) {
  unloadNamespace(package)
}

rm(list=ls(all=TRUE))

# load library
library(synthpop)
library(tidyverse)
library(xtable)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
graphs = "graphs/"
tables = "tables/"

setwd(main_dir)

#functions
options(scipen=999) 

# Load data ----

ods <- SD2011[, c("sex", "age", "region","placesize","depress")]
ods <- ods[sample(1:nrow(ods)),]
    
# original ----
s5 <- syn(ods, seed = 8564, m = 5, print.flag = FALSE)

t5 <- disclosure( s5, ods, keys = c("sex", "age", "region", "placesize"), target = "depress", print.flag = FALSE)

ttest1 <- print(t5, to.print = "allCAPs")

repU <- t5$ident$repU
average_row <- mean(repU) # calculate average row across 10 synthetic data sets
repU <- c(t5$ident$UiO[1], repU, average_row)

DiSCO <- t5$attrib$DiSCO
average_row <- mean(DiSCO) # calculate average row across 10 synthetic data sets
DiSCO <- c(t5$attrib$Dorig[1], DiSCO, average_row)

# create table
df_risk_1 <- data.frame(
  data = c("Original data", "Synthetic 1", "Synthetic 2", "Synthetic 3", "Synthetic 4", "Synthetic 5", "Average"),
  identity = c(repU),
  attribute = c(DiSCO)
)

df_risk_1
