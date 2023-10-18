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
main_dir = "/Users/jonathanlatner/Documents/GitHub/IAB/simulation_data/continuous_dim/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

setwd(main_dir)


# compare ----

df_specks <- data.frame(data = as.character(),
                        rows = as.numeric(),
                        cols = as.numeric(),
                        specks = as.numeric())

rows = c(50000, 100000,200000) # Rows/observations
cols = c(10, 15, 20) # Columns/variables
data = c("synthpop","datasynthesizer")

for (r in rows) {
  for (c in cols) {
    for (d in data) {
      
      df_ods <- read.csv(paste0(original_data,"ods_rows_",r,"_cols_",c,".csv"))
      df_sds <- read.csv(paste0(synthetic_data,d,"/sds_",d,"_rows_",r,"_cols_",c,"_n_1.csv"))
      df_synds <- readRDS(paste0(data_files,"synthetic/synds_rows_",r,"_cols_",c,"_m_1.rds"))
      df_synds$syn <- df_sds
      
      utility_measure <- utility.gen(df_synds$syn, df_ods, print.stats = "SPECKS", nperms = 3)
      
      specks <- data.frame(data = d,
                           rows = r,
                           cols = c,
                           specks = as.numeric(utility_measure$SPECKS[1]))
      df_specks <- rbind(df_specks,specks)
    }
  }
}

df_specks

write.csv(df_specks, paste0(tables,"utility_specks.csv"), row.names=FALSE)

  