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

# Set seed for reproducibility
set.seed(123)

# v00 - baseline data ----

ods <- SD2011
ods$bmi <- NULL
ods$agegr <- NULL
ods[ods < 0] <- NA
ods[ods == ""] <- NA

write.csv(ods, paste0(original_data,"sd2011_v00.csv"), row.names=FALSE)

# v01 - without eduspec or wkabdur ----


df_ods <- ods
df_ods$eduspec <- NULL
df_ods$wkabdur <- NULL

write.csv(df_ods, paste0(original_data,"sd2011_v01.csv"), row.names=FALSE)

# v02 - eduspec is last and without wkabdur ----


df_ods <- ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))
df_ods$wkabdur <- NULL

write.csv(df_ods, paste0(original_data,"sd2011_v02.csv"), row.names=FALSE)

# v03 - wkabdur is last and without eduspec ----


df_ods <- ods %>%
  select(-wkabdur, wkabdur) %>%
  arrange(desc(wkabdur))
df_ods$eduspec <- NULL

write.csv(df_ods, paste0(original_data,"sd2011_v03.csv"), row.names=FALSE)

# v04 eduspec, wkabdur  are last ----


df_ods <- ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods <- df_ods %>%
  select(-wkabdur, wkabdur) %>%
  arrange(desc(wkabdur))

write.csv(df_ods, paste0(original_data,"sd2011_v04.csv"), row.names=FALSE)

# v05 wkabdur, eduspec are last ----


df_ods <- ods %>%
  select(-wkabdur, wkabdur) %>%
  arrange(desc(wkabdur))

df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

write.csv(df_ods, paste0(original_data,"sd2011_v05.csv"), row.names=FALSE)

# v06 - eduspec is last and wkabdur is numeric ----


df_ods <- ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods$wkabdur <- as.numeric(df_ods$wkabdur)

write.csv(df_ods, paste0(original_data,"sd2011_v06.csv"), row.names=FALSE)

# v07 - wkabdur, random ar last ----
# random has 35 values


df_ods <- ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

# Create a random variable with 5000 cases that XX unique character values
unique_values <- sample(letters2, 35, replace = FALSE)
df_ods$random <- sample(unique_values, nrow(df_ods), replace = TRUE)

write.csv(df_ods, paste0(original_data,"sd2011_v07_35.csv"), row.names=FALSE)

# v07 - wkabdur, random ar last ----
# random has 40 values


df_ods <- ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

# Create a random variable with 5000 cases that XX unique character values
unique_values <- sample(letters2, 40, replace = FALSE)
df_ods$random <- sample(unique_values, nrow(df_ods), replace = TRUE)

write.csv(df_ods, paste0(original_data,"sd2011_v07_40.csv"), row.names=FALSE)

# v08 - wkabdur, random ar last ----
# 2 random variables has 40 values


df_ods <- ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

# Create a random variable with 5000 cases that XX unique character values
unique_values <- sample(letters2, 25, replace = FALSE)
df_ods$random_1 <- sample(unique_values, nrow(df_ods), replace = TRUE)

unique_values <- sample(letters2, 25, replace = FALSE)
df_ods$random_2 <- sample(unique_values, nrow(df_ods), replace = TRUE)

write.csv(df_ods, paste0(original_data,"sd2011_v08.csv"), row.names=FALSE)
