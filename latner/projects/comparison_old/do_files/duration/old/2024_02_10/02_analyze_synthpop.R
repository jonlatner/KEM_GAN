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

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/ctgan/"
tables = "tables/ctgan/"
duration = "duration/analyze/"

setwd(main_dir)

# synthpop ----

synthpop_csv_1 <- read.csv(paste0(duration,"duration_synthpop_sd2011_csv_analyze.csv")) %>%
  rename(sdg=type) %>%
  mutate(sdg="synthpop (csv 1)")
synthpop_csv_2 <- read.csv(paste0(duration,"duration_synthpop_sd2011_csv_analyze_2.csv")) %>%
  rename(sdg=type) %>%
  mutate(sdg="synthpop (csv 2)")
synthpop_package <- read.csv(paste0(duration,"duration_synthpop_sd2011_package_analyze.csv")) %>%
  rename(sdg=type) %>%
  mutate(sdg="synthpop (package)")

df_synthpop <- rbind(synthpop_csv_1,synthpop_csv_2,synthpop_package)
df_synthpop
