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
my.seed = 1237
set.seed(my.seed)

# Load original data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))

# Create synthetic data with numeric variables ----

sds <- syn(df_ods, m = 1, seed = my.seed)
sds <- sds$syn
write.csv(sds,paste0(synthetic_data,"synthpop_numeric.csv"), row.names = FALSE)

# Create synthetic data with factor variables ----

sds <- syn(df_ods, m = 1, seed = my.seed, minnumlevels = 5)
sds <- sds$syn
write.csv(sds,paste0(synthetic_data,"synthpop_factor.csv"), row.names = FALSE)


