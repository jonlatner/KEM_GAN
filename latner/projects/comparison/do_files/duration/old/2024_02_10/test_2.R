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
library(beepr)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
duration = "duration/"

setwd(main_dir)

#functions
options(scipen=999) 

# create synthetic data from csv ----

ods <- SD2011
df_ods <- ods

df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

# Set seed for reproducibility (optional)
set.seed(123)

# Create a random variable with 5000 cases, each containing XX unique character values
unique_values <- sample(letters, 26, replace = FALSE)
df_ods$random <- sample(unique_values, nrow(df_ods), replace = TRUE)

write.csv(df_ods, paste0(original_data,"test.csv"), row.names = FALSE)

df_time_duration <- data.frame()
data <- c("test")
for (d in data) {
  
  ods <- read.csv(file = paste0(original_data,d,".csv"))
  
  ods <- ods %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.integer, as.numeric)
  
  df_ods <- ods

  time_start <- proc.time()
  df_synds <- syn(df_ods, m = 1) 
  time_end <- proc.time()
  time_duration <- as.numeric(time_end[1] - time_start[1])
}
df_time_duration_csv <- time_duration
print(df_time_duration_csv)
