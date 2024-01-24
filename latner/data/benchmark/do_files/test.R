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

setwd(main_dir)

#functions
options(scipen=999) 

# Create fake synthetic data ----

library(synthpop)
df_ods <- SD2011
df_ods_new <- df_ods
# df_ods_new <- select(df_ods_new,sex,age,wkabint,wkabintdur,eduspec,englang,everything())
# df_ods_new$eduspec <- NULL
# df_ods_new$emcc <- NULL
# df_ods_new$region <- NULL

time_start <- proc.time()
df_synds <- syn(df_ods_new, m = 1)
time_end <- proc.time()
time_duration <- as.numeric(time_end[1] - time_start[1])
time_duration


time_start <- proc.time()
df_ods_1 <- select(df_ods,alcabuse,eduspec)
df_synds <- syn(df_ods_1, m = 1)
time_end <- proc.time()
time_duration_1 <- as.numeric(time_end[1] - time_start[1])

time_start <- proc.time()
df_ods_2 <- select(df_ods,eduspec,alcabuse)
df_synds <- syn(df_ods_2, m = 1)
time_end <- proc.time()
time_duration_2 <- as.numeric(time_end[1] - time_start[1])

print(time_duration_1)
print(time_duration_2)

