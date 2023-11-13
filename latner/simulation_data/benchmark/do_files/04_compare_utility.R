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
library(tidyverse)
library(xtable)
library(synthpop)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/simulation_data/benchmark/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
graphs = "graphs/"
tables = "tables/"

#functions
options(scipen=999) 

# Load utility baseline data ----


copies <- c(1)
data <- c("adult","grid","gridr","sd2011_small")
data <- c("sd2011_small")
type <- c("synthpop","ctgan")
for (d in data) {
  for (t in type) {
    for (c in copies) {
      df_ods <- read.csv(paste0(original_data,d,".csv"))
      sds_synds <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
      sds <- read.csv(paste0(synthetic_data,"sds_synthpop_",d,".csv"))
      # sds <- sds %>%
      #   mutate_if(is.character, as.factor)
      sds_synds$syn <- sds

    } 
    df_compare <- compare(sds_synds, df_ods)
    df_compare <- data.frame(df_compare$tables) %>%
      rownames_to_column(var = "data") %>%
      pivot_longer(cols = starts_with(names(df_ods))) %>%
      rename(pct = value) %>%
      separate(name, into = c("variable", "value"), sep = "\\.", remove = FALSE) %>%
      select(-name)
  }
}


