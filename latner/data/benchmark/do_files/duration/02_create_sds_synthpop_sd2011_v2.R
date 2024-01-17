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

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
duration_folder = "duration/"

setwd(main_dir)

#functions
options(scipen=999) 

# Create fake synthetic data with 1 copy (and time duration) ----

df_ods <- read.csv(paste0(original_data,"SD2011.csv"))
df_ods[df_ods == ""] <- NA
df_ods <- df_ods %>%
  mutate_if(is.character, as.factor)

# Identify columns with numeric (continuous) variables
numeric_cols <- df_ods %>%
  select_if(is.numeric)

df_new <- numeric_cols[1:1]

# add variables
for (var in c("eduspec","sex","alcabuse")) {
  df_new[[var]] <- df_ods[[var]]
}
summary(df_new)

# df_new <- na.omit(df_new)

df_synds <- syn(df_new, m = 1)
