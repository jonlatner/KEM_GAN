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
library(ggcorrplot)
library(fastDummies)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
tables = "tables/"

setwd(main_dir)

#functions
options(scipen=999) 

# Create fake synthetic data ----

ods <- SD2011
ods[ods == ""] <- NA
ods[ods < 0] <- NA

df_synthpop <- read.csv(paste0(synthetic_data,"synthpop/sds_synthpop_sd2011_clean_m_1_n_1.csv"))
df_datasynthesizer <- read.csv(paste0(synthetic_data,"datasynthesizer/sds_datasynthesizer_sd2011_clean_k_1_e_0_m_1_n_1.csv"))
df_ctgan <- read.csv(paste0(synthetic_data,"ctgan/sds_ctgan_data_sd2011_clean_epochs_50_m_1_n_1.csv"))

# Correlation plots ----
df_corr <- ods %>%
  filter(!is.na(edu)) %>%
  select(age,height,weight,bmi,sex,edu,income) 
df_corr <- dummy_cols(df_corr, select_columns = c("sex","edu"), remove_selected_columns = TRUE)
corr_ods <- cor(df_corr,use = "pairwise.complete.obs")

df_corr <- df_synthpop %>%
  filter(!is.na(edu)) %>%
  select(age,height,weight,bmi,sex,edu,income) 
df_corr <- dummy_cols(df_corr, select_columns = c("sex","edu"), remove_selected_columns = TRUE)
corr_synthpop <- cor(df_corr,use = "pairwise.complete.obs")

df_corr <- df_datasynthesizer %>%
  filter(!is.na(edu)) %>%
  select(age,height,weight,bmi,sex,edu,income) 
df_corr <- dummy_cols(df_corr, select_columns = c("sex","edu"), remove_selected_columns = TRUE)
corr_datasynthesizer <- cor(df_corr,use = "pairwise.complete.obs")

df_corr <- df_ctgan %>%
  filter(!is.na(edu)) %>%
  select(age,height,weight,bmi,sex,edu,income) 
df_corr <- dummy_cols(df_corr, select_columns = c("sex","edu"), remove_selected_columns = TRUE)
corr_ctgan <- cor(df_corr,use = "pairwise.complete.obs")

# Graph ----

ggcorrplot(corr_ods, lab = TRUE, type = "lower")
ggcorrplot(corr_synthpop, lab = TRUE, type = "lower")
ggcorrplot(corr_datasynthesizer, lab = TRUE, type = "lower")
ggcorrplot(corr_ctgan, lab = TRUE, type = "lower")
