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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
duration = "duration/"

setwd(main_dir)

#functions
options(scipen=999) 

# load data ----

ods <- read.csv(file = paste0(original_data,"sd2011_v07.csv"))

# analysis ----

df_ods <- ods

df_ods <- df_ods %>%
  mutate_if(is.character, as.factor)

ods <- SD2011
ods <- ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))
ods$wkabdur <- as.numeric(ods$wkabdur)

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_7 <- as.numeric(time_end[1] - time_start[1])
time_duration_7$type <- "as.numeric(wkabdur) and last variable: eduspec"
time_duration_7

# save ----

