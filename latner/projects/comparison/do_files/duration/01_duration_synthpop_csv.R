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
duration = "duration/analyze/"

setwd(main_dir)

#functions
options(scipen=999) 

# create synthetic data ----

data <- c("sd2011_v00","sd2011_v01","sd2011_v02","sd2011_v03","sd2011_v04","sd2011_v05","sd2011_v06","sd2011_v07_35","sd2011_v07_40","sd2011_v08")
for (d in data) {

  print(d)
  ods <- read.csv(file = paste0(original_data,d,".csv"))
  
  df_ods <- ods
  time_start <- proc.time()
  df_synds <- syn(df_ods, m = 1) 
  time_end <- proc.time()
  df_duration <- data.frame(
    duration = as.numeric(time_end[1] - time_start[1]),
    data = d,
    type = "synthpop (csv)"
  )

  print(df_duration)
  
  write.csv(df_duration, paste0(duration,"duration_synthpop_",d,".csv"), row.names=FALSE)
}

beep()
