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
my.seed = 1237 # reproduces 1 observation
# my.seed = 1238 # reproduces 0 observation
# my.seed = 1240 # reproduces 3 observation
set.seed(my.seed)

# load original data ----


# Load original data ----
ods <- read.csv(paste0(original_data,"simulated.csv"))

# Create fake synthetic data ----

sds <- syn(ods, m = 1, seed = my.seed,models = TRUE, cart.cp = 0.01)

model_categorical <- sds$models$var4
rpart.plot(model_categorical, roundint = FALSE)

model_categorical <- sds$models$var4$frame
model_categorical

sds <- sds$syn
