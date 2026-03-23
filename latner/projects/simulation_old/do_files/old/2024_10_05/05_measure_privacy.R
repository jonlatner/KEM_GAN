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
library(xtable)

# https://arxiv.org/pdf/2406.16826

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
tables = "tables/"

setwd(main_dir)

#functions
options(scipen=999) 

# Set seed for reproducibility
my.seed = 1237
my.seed = 1240 # reproduces 3 observation

set.seed(my.seed)

# Load original data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))

# Create synthetic data with numeric variables ----

sds <- syn(df_ods, m = 1, seed = my.seed)

# Identity disclosure measures ----

t1 <- disclosure.summary(sds, df_ods, print.flag = FALSE, plot = TRUE, keys = c("var1", "var2", "var3"), target = "var4")
print(t1, plot = FALSE, to.print = "ident")
print(t1, plot = FALSE, to.print = "attrib")
replicated.uniques (sds, df_ods)


