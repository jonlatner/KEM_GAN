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

# load data ----

ods <- read.csv(file = paste0(original_data,"sd2011.csv"))

# analysis ----

df_ods <- ods
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))
df_ods$eduspec <- NULL
df_ods$wkabdur <- NULL

write.csv(df_ods, paste0(original_data,"sd2011_v01.csv"), row.names=FALSE)

# analysis ----

df_ods <- ods
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))
df_ods$wkabdur <- NULL

write.csv(df_ods, paste0(original_data,"sd2011_v02.csv"), row.names=FALSE)

# analysis ----

df_ods <- ods
df_ods <- df_ods %>%
  select(-wkabdur, wkabdur) %>%
  arrange(desc(wkabdur))
df_ods$eduspec <- NULL

write.csv(df_ods, paste0(original_data,"sd2011_v03.csv"), row.names=FALSE)

# analysis ----

df_ods <- ods
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods <- df_ods %>%
  select(-wkabdur, wkabdur) %>%
  arrange(desc(wkabdur))

write.csv(df_ods, paste0(original_data,"sd2011_v04.csv"), row.names=FALSE)

# analysis ----

df_ods <- ods

df_ods <- df_ods %>%
  select(-wkabdur, wkabdur) %>%
  arrange(desc(wkabdur))

df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

write.csv(df_ods, paste0(original_data,"sd2011_v05.csv"), row.names=FALSE)

# analysis ----

df_ods <- ods

df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods$wkabdur <- as.numeric(df_ods$wkabdur)

write.csv(df_ods, paste0(original_data,"sd2011_v06.csv"), row.names=FALSE)


# analysis ----

df_ods <- ods

df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

# Set seed for reproducibility (optional)
set.seed(123)

# Create a random variable with 5000 cases, each containing 26 unique character values
df_ods$random <- sample(letters, nrow(df_ods), replace = TRUE)

# Introduce some missing values randomly
missing_indices <- sample(1:nrow(df_ods), 0.01 * nrow(df_ods))  # Adjust 0.2 based on the desired proportion of missing values
# df_ods$random[missing_indices] <- NA
summary(df_ods)
table(df_ods$random,useNA = "ifany")
table(df_ods$eduspec,useNA = "ifany")

write.csv(df_ods, paste0(original_data,"sd2011_v07.csv"), row.names=FALSE)
