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
tables = "tables/ctgan/"

setwd(main_dir)

# compare ----

df_specks <- data.frame(epochs = as.numeric(),
                        rows = as.numeric(),
                        cols = as.numeric(),
                        batch = as.numeric(),
                        specks = as.numeric())

rows = c("200000") # Rows/observations
cols = c(15) # Columns/variables
epochs = c(10, 20, 30, 40, 50, 75, 100)
batch = c(500, 1000, 5000, "10000")


for (r in rows) {
  for (c in cols) {
    for (e in epochs) {
      for (b in batch) {
        
        print(paste(r, c, e, b, sep =","))
        
        #load original data
        df_ods <- read.csv(paste0(original_data,"ods_rows_",r,"_cols_",c,".csv"))
        
        #load synds object
        df_synds <- readRDS(paste0(data_files,"synthetic/synds_rows_",r,"_cols_",c,"_m_1.rds"))

        #load synthetic data
        sds <- read.csv(paste0(synthetic_data,"ctgan/sds_ctgan_rows_",r,"_cols_",c,"_n_1_epochs_",e,"_batch_",b,".csv"))

        #replace synds object with synthetic data
        df_synds$syn <- sds

        #compare
        utility_measure <- utility.gen(df_synds$syn, df_ods, print.stats = "SPECKS", nperms = 3)
        
        specks <- data.frame(epochs = e,
                             rows = r,
                             cols = c,
                             batch = b,
                             specks = as.numeric(utility_measure$SPECKS[1]))
        df_specks <- rbind(df_specks,specks)
      }
    }
  }
}

df_specks %>% arrange(batch, epochs)

write.csv(df_specks, paste0(tables,"utility_ctgan_specks_rows_200000_cols_15.csv"), row.names=FALSE)

  