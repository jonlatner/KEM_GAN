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
library(ggh4x) # facet_nested

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"

setwd(main_dir)

#functions
options(scipen=999) 

# Set seed for reproducibility
my.seed = 1234
set.seed(my.seed)

# Create fake synthetic data with m copies ----

copies <- c(1)
data <- c("simulated")
for (d in data) {
  print(d)
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  
  for (c in copies) {
    
    df_synds <- syn(df_ods, m = c, seed = my.seed + 1)
    
    # save RDS file for future use with synthpop package (i.e. utility measures)
    saveRDS(df_synds, paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
    
    # save csv file
    for (j in 1:c) {
      # synthpop_df <- df_synds$syn[[j]] # if m > 1
      synthpop_df <- df_synds$syn # if m == 1
      write.csv(synthpop_df, file = paste0(synthetic_data,"sds_synthpop_",d,"_m_",c,"_n_",j,".csv"), row.names = FALSE)
    }
  }
}


