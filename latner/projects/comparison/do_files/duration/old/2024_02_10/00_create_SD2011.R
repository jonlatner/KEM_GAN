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
duration = "duration/"

setwd(main_dir)

#functions
options(scipen=999) 

# Generate all combinations for a vector of letters from A to Z
letters_vector <- LETTERS
combinations <- expand.grid(letters_vector, letters_vector)
letters2 <- apply(combinations, 1, paste, collapse = "")

# v00 - baseline data ----

ods <- SD2011
write.csv(ods, paste0(original_data,"sd2011_v00.csv"), row.names=FALSE)

# v01 - without eduspec or wkabdur ----

ods <- SD2011
df_ods <- ods
df_ods$eduspec <- NULL
df_ods$wkabdur <- NULL

write.csv(df_ods, paste0(original_data,"sd2011_v01.csv"), row.names=FALSE)

# v02 - eduspec is last and without wkabdur ----

ods <- SD2011
df_ods <- ods
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))
df_ods$wkabdur <- NULL

write.csv(df_ods, paste0(original_data,"sd2011_v02.csv"), row.names=FALSE)

# v03 - wkabdur is last and without eduspec ----

ods <- SD2011
df_ods <- ods
df_ods <- df_ods %>%
  select(-wkabdur, wkabdur) %>%
  arrange(desc(wkabdur))
df_ods$eduspec <- NULL

write.csv(df_ods, paste0(original_data,"sd2011_v03.csv"), row.names=FALSE)

# v04 eduspec, wkabdur  are last ----

ods <- SD2011
df_ods <- ods
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods <- df_ods %>%
  select(-wkabdur, wkabdur) %>%
  arrange(desc(wkabdur))

write.csv(df_ods, paste0(original_data,"sd2011_v04.csv"), row.names=FALSE)

# v05 wkabdur, eduspec are last ----

ods <- SD2011
df_ods <- df_ods %>%
  select(-wkabdur, wkabdur) %>%
  arrange(desc(wkabdur))

df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

write.csv(df_ods, paste0(original_data,"sd2011_v05.csv"), row.names=FALSE)

# v06 - eduspec is last and wkabdur is numeric ----

ods <- SD2011
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods$wkabdur <- as.numeric(df_ods$wkabdur)

write.csv(df_ods, paste0(original_data,"sd2011_v06.csv"), row.names=FALSE)

# v07 - wkabdur, random ar last ----
# random has 35 values

ods <- SD2011
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

# Set seed for reproducibility
set.seed(123)

# Create a random variable with 5000 cases that XX unique character values
unique_values <- sample(letters2, 35, replace = FALSE)
df_ods$random <- sample(unique_values, nrow(df_ods), replace = TRUE)

write.csv(df_ods, paste0(original_data,"sd2011_v07_35.csv"), row.names=FALSE)

# v07 - wkabdur, random ar last ----
# random has 40 values

ods <- SD2011
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

# Set seed for reproducibility
set.seed(123)

# Create a random variable with 5000 cases that XX unique character values
unique_values <- sample(letters2, 40, replace = FALSE)
df_ods$random <- sample(unique_values, nrow(df_ods), replace = TRUE)

write.csv(df_ods, paste0(original_data,"sd2011_v07_40.csv"), row.names=FALSE)
