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

# baseline ----

df_time_duration <- data.frame()
data <- c("sd2011","sd2011_v01","sd2011_v02","sd2011_v03","sd2011_v04","sd2011_v05","sd2011_v06","sd2011_v07")
for (d in data) {
  
  ods <- read.csv(file = paste0(original_data,d,".csv"))
  
  df_ods <- ods
  time_start <- proc.time()
  df_synds <- syn(df_ods, m = 1) 
  time_end <- proc.time()
  raw_duration <- data.frame(
    duration = as.numeric(time_end[1] - time_start[1]),
    data = d
  )
  df_time_duration <- rbind(df_time_duration,raw_duration)
}

df_time_duration$type <- c("raw",
                           "without eduspec or wkabdur",
                           "without wkabdur",
                           "without eduspec",
                           "last variables: eduspec-wkabdur",
                           "last variables: wkabdur-eduspec",
                           "as.numeric(wkabdur) and last variable: eduspec",
                           "last variable: eduspec and random"
                           ) 

# save ----


write.csv(df_time_duration, paste0(duration,"duration_synthpop_sd2011_csv_analyze.csv"), row.names=FALSE)
