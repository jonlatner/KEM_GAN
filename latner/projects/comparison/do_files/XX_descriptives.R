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
library(ggh4x) # facet_nested

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/datasynthesizer/"
tables = "tables/datasynthesizer/"

setwd(main_dir)


# Load data ----

d <- c("sd2011")
df_ods <- read.csv(paste0(original_data,d,".csv")) # load original data

table(df_ods$wkabdur)

# Graph (income) ----

test <- df_ods %>%
  group_by(age)%>%
  summarise(income=mean(income,na.rm = TRUE))%>%
  ungroup()
test

test <- df_ods %>%
  filter(age>16)
summary(test$income)
