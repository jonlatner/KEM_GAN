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

# FOLDERS - ADAPT THIS PATHWAY
# main_dir = "N:/Ablagen/D01700-KEM/Latner/little_etal_2021/"
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/simulation_data/benchmark/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"

setwd(main_dir)

#functions
options(scipen=999) 

# Create fake synthetic data ----

copies <- c(1)
data <- c("adult","census","grid","gridr")
for (d in data) {
  print(d)
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  for (c in copies) {
    df_synds <- syn(df_ods, m = c)
    saveRDS(df_synds, paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
  }
}

