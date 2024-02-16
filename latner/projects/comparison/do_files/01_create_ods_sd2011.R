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

setwd(main_dir)

#functions
options(scipen=999) 

# Create fake synthetic data ----

ods <- SD2011
write.csv(ods, paste0(original_data,"sd2011.csv"), row.names = FALSE)

ods <- SD2011
ods[ods < 0] <- NA
ods[ods == ""] <- NA
ods <- ods %>%
  mutate_if(is.character, as.factor)
write.csv(ods, paste0(original_data,"sd2011_clean.csv"), row.names = FALSE)

ods <- SD2011
ods[ods < 0] <- NA
ods[ods == ""] <- NA
ods <- ods %>%
  mutate_if(is.character, as.factor)
ods$agegr <- NULL
ods$bmi <- NULL
write.csv(ods, paste0(original_data,"sd2011_clean_small.csv"), row.names = FALSE)
