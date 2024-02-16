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

ods <- SD2011

# baseline ----

df_ods <- ods
time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_0 <- as.numeric(time_end[1] - time_start[1])
time_duration_0
time_duration_0$type <- "raw"

# analysis ----

df_ods <- ods
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))
df_ods$eduspec <- NULL
df_ods$wkabdur <- NULL

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_1 <- as.numeric(time_end[1] - time_start[1])
time_duration_1$type <- "without eduspec or wkabdur"
time_duration_1

# analysis ----

df_ods <- ods
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))
df_ods$wkabdur <- NULL

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_2 <- as.numeric(time_end[1] - time_start[1])
time_duration_2$type <- "without wkabdur"
time_duration_2

# analysis ----

df_ods <- ods
df_ods <- df_ods %>%
  select(-wkabdur, wkabdur) %>%
  arrange(desc(wkabdur))
df_ods$eduspec <- NULL

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_3 <- as.numeric(time_end[1] - time_start[1])
time_duration_3$type <- "without eduspec"
time_duration_3

# analysis ----

df_ods <- ods
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods <- df_ods %>%
  select(-wkabdur, wkabdur) %>%
  arrange(desc(wkabdur))

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_4 <- as.numeric(time_end[1] - time_start[1])
time_duration_4$type <- "last variables: eduspec-wkabdur"
time_duration_4

# analysis ----

df_ods <- ods

df_ods <- df_ods %>%
  select(-wkabdur, wkabdur) %>%
  arrange(desc(wkabdur))

df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_5 <- as.numeric(time_end[1] - time_start[1])
time_duration_5$type <- "last variables: wkabdur-eduspec"
time_duration_5

# analysis ----

df_ods <- ods

df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods$wkabdur <- as.numeric(df_ods$wkabdur)

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_6 <- as.numeric(time_end[1] - time_start[1])
time_duration_6$type <- "as.numeric(wkabdur) and last variable: eduspec"
time_duration_6

# analysis ----

df_ods <- ods

df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods$wkabdur <- as.numeric(df_ods$wkabdur)

# Set seed for reproducibility (optional)
set.seed(123)

# Create a random variable with 5000 cases, each containing 26 unique character values
df_ods$random <- sample(letters, nrow(df_ods), replace = TRUE)

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_7 <- as.numeric(time_end[1] - time_start[1])
time_duration_7$type <- "last variable: eduspec and random"
time_duration_7

# save ----

time_duration_0
time_duration_1
time_duration_2
time_duration_3
time_duration_4
time_duration_5
time_duration_6
time_duration_7

time_duration <- rbind(time_duration_0,time_duration_1,time_duration_2,time_duration_3,time_duration_4,time_duration_5,time_duration_6,time_duration_7)
time_duration

write.csv(time_duration, paste0(duration,"duration_synthpop_sd2011_analyze.csv"), row.names=FALSE)
