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

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/datasynthesizer/"
tables = "tables/datasynthesizer/"

#functions
options(scipen=999) 

# Load utility from datasynthesizer data ----

ods <- SD2011
ods <- ods %>%
  select(age,edu,sex,income)

sds <- syn(ods,m = 1)

compare <- compare(sds,ods,utility.stats = "SPECKS")
mean(compare$tab.utility[,1])

utility <- utility.gen(sds$syn, ods, print.stats = "SPECKS", nperms = 3)

mean(as.numeric(compare$SPECKS))
