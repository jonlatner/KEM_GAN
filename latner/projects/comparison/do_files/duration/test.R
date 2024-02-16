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
duration = "duration/analyze/"

setwd(main_dir)

#functions
options(scipen=999) 

# Generate all combinations for a vector of letters from A to Z
letters_vector <- LETTERS
combinations <- expand.grid(letters_vector, letters_vector)
letters2 <- apply(combinations, 1, paste, collapse = "")

# v08 - wkabdur, random ar last ----
# random has XX values

ods <- SD2011
df_ods <- ods

df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods$wkabdur <- as.numeric(df_ods$wkabdur)

# Set seed for reproducibility
set.seed(123)

# Create a random variable with 5000 cases that XX unique character values
unique_values <- sample(letters2, 25, replace = FALSE)
df_ods$random_1 <- sample(unique_values, nrow(df_ods), replace = TRUE)

unique_values <- sample(letters2, 25, replace = FALSE)
df_ods$random_2 <- sample(unique_values, nrow(df_ods), replace = TRUE)

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_8 <- as.numeric(time_end[1] - time_start[1])
time_duration_8

df_time_duration <- data.frame(data = c("sd2011_v08"),
                               duration = c(time_duration_8),
                               type = "synthpop")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_sd2011_v08_package.csv"), row.names=FALSE)
