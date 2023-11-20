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
library(synthpop)
library(ggh4x) # facet_nested

# FOLDERS - ADAPT THIS PATHWAY
# main_dir = "N:/Ablagen/D01700-KEM/Latner/little_etal_2021/"
main_dir = "/Users/jonathanlatner/Documents/GitHub/IAB/simulation_data/categorical_dim/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/ctgan/"

setwd(main_dir)


# Create fake synthetic data ----

rows = c(1000, 5000) # Rows/observations
cols = c(10, 15) # Columns/variables
vals = c(20) # Columns/variables
for (r in rows) {
  for (c in cols) {
    for (v in vals) {
      df_ods <- read.csv(paste0(original_data,"ods_rows_",r,"_cols_",c,"_vals_",v,".csv"))
      df_synds <- syn(df_ods, m = 1, seed = 1234)
      saveRDS(df_synds, paste0(synthetic_data,"df_synds_rows_",r,"_cols_",c,"_vals_",v,".rds"))
    }
  }
}

