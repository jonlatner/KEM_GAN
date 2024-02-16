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
duration_folder = "duration/SD2011/"

setwd(main_dir)

#functions
options(scipen=999) 

# Open SD2011 ----

ods <- SD2011

# example ----

df_ods <- ods[, 1:10]
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_0 <- as.numeric(time_end[1] - time_start[1])
time_duration_0

# example ----

df_ods <- ods[, 1:20]
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_1 <- as.numeric(time_end[1] - time_start[1])
time_duration_1

# example ----

df_ods <- ods[, 1:25]
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_2 <- as.numeric(time_end[1] - time_start[1])
time_duration_2

# example ----

df_ods <- ods[, 1:27]
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_3 <- as.numeric(time_end[1] - time_start[1])
time_duration_3

# example ----

df_ods <- ods
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))
df_ods$wkabdur <- NULL

# df_ods[df_ods < 0] <- NA
# df_ods[df_ods == ""] <- NA
# df_ods <- df_ods %>%
#   mutate_if(is.character, as.numeric)
# summary(df_ods$wkabdur)

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_4 <- as.numeric(time_end[1] - time_start[1])
time_duration_4

# example ----

df_ods <- ods
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods <- df_ods %>%
  select(-wkabdur, wkabdur) %>%
  arrange(desc(wkabdur))

df_ods$wkabdur <- as.factor(df_ods$wkabdur)
table(df_ods$wkabdur)

# df_ods[df_ods < 0] <- NA
# df_ods[df_ods == ""] <- NA
# df_ods <- df_ods %>%
#   mutate_if(is.character, as.numeric)
# summary(df_ods$wkabdur)

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_5 <- as.numeric(time_end[1] - time_start[1])
time_duration_5
