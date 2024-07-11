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
duration = "duration/"

setwd(main_dir)

#functions
options(scipen=999) 

# Generate all combinations for a vector of letters from A to Z
letters_vector <- LETTERS
combinations <- expand.grid(letters_vector, letters_vector)
letters2 <- apply(combinations, 1, paste, collapse = "")

# create synthetic data from package ----

library(synthpop)
ods <- SD2011
df_ods <- ods

df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods$wkabdur <- as.numeric(df_ods$wkabdur)

# Set seed for reproducibility (optional)
set.seed(123)

# Create a random variable with 5000 case that containing XX unique character values
unique_values <- sample(letters2, 25, replace = FALSE)
df_ods$random <- sample(unique_values, nrow(df_ods), replace = TRUE)

table(df_ods$random, useNA = "ifany")

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration <- as.numeric(time_end[1] - time_start[1])
df_time_duration_pkg_25 <- time_duration
beep()

# Create a random variable with 5000 case that containing XX unique character values
unique_values <- sample(letters2, 30, replace = FALSE)
df_ods$random <- sample(unique_values, nrow(df_ods), replace = TRUE)

table(df_ods$random, useNA = "ifany")

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration <- as.numeric(time_end[1] - time_start[1])
df_time_duration_pkg_30 <- time_duration
beep()

# Create a random variable with 5000 case that containing XX unique character values
unique_values <- sample(letters2, 35, replace = FALSE)
df_ods$random <- sample(unique_values, nrow(df_ods), replace = TRUE)

table(df_ods$random, useNA = "ifany")

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration <- as.numeric(time_end[1] - time_start[1])
df_time_duration_pkg_35 <- time_duration
beep()

# Create a random variable with 5000 case that containing XX unique character values
unique_values <- sample(letters2, 40, replace = FALSE)
df_ods$random <- sample(unique_values, nrow(df_ods), replace = TRUE)

table(df_ods$random, useNA = "ifany")

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration <- as.numeric(time_end[1] - time_start[1])
df_time_duration_pkg_40 <- time_duration
beep()

df_time_duration_pkg_25
df_time_duration_pkg_30
df_time_duration_pkg_35
df_time_duration_pkg_40

