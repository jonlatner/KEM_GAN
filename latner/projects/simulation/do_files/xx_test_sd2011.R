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
library(rpart.plot)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
graphs = "graphs/"
original_data = "data_files/original/"
synthpop_data = "data_files/synthetic/synthpop/"
datasynthesizer_data = "data_files/synthetic/datasynthesizer/"

setwd(main_dir)

#functions
options(scipen=999) 

# Set seed for reproducibility
my.seed = 1237
set.seed(my.seed)

# Load original data ----


ods <- SD2011
ods <- SD2011[,c("sex","age","edu","income","bmi")]
# ods <- SD2011[,!names(SD2011) %in% c("eduspec","wkabdur")]

# Create fake synthetic data ----

sds <- syn(ods, m = 1, seed = my.seed,models = TRUE)
replicated.uniques (sds, ods)


t1 <- disclosure.summary(sds, ods, print.flag = FALSE, plot = TRUE, keys = c("sex","age","edu"), target = "income")
print(t1,to.print = "ident")
print(t1,to.print = "attrib")
