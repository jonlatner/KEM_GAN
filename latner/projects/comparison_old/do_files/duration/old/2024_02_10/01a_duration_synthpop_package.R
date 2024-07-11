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

# baseline ----

ods <- SD2011
df_ods <- ods
time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_0 <- as.numeric(time_end[1] - time_start[1])
time_duration_0

df_time_duration <- data.frame(data = c("sd2011_v00"),
                               duration = c(time_duration_0),
                               type = "synthpop")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_sd2011_v00_package_analyze.csv"), row.names=FALSE)

# v01 - without eduspec or wkabdur ----

ods <- SD2011
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
time_duration_1

df_time_duration <- data.frame(data = c("sd2011_v01"),
                               duration = c(time_duration_1),
                               type = "synthpop")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_sd2011_v01_package_analyze.csv"), row.names=FALSE)

# v02 - eduspec is last and without wkabdur ----

ods <- SD2011
df_ods <- ods
df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))
df_ods$wkabdur <- NULL

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_2 <- as.numeric(time_end[1] - time_start[1])
time_duration_2

df_time_duration <- data.frame(data = c("sd2011_v02"),
                               duration = c(time_duration_2),
                               type = "synthpop")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_sd2011_v02_package_analyze.csv"), row.names=FALSE)

# v03 - wkabdur is last and without eduspec ----

ods <- SD2011
df_ods <- ods
df_ods <- df_ods %>%
  select(-wkabdur, wkabdur) %>%
  arrange(desc(wkabdur))
df_ods$eduspec <- NULL

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_3 <- as.numeric(time_end[1] - time_start[1])
time_duration_3

df_time_duration <- data.frame(data = c("sd2011_v03"),
                               duration = c(time_duration_3),
                               type = "synthpop")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_sd2011_v03_package_analyze.csv"), row.names=FALSE)

# v04 eduspec, wkabdur  are last ----

ods <- SD2011
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
time_duration_4

df_time_duration <- data.frame(data = c("sd2011_v04"),
                               duration = c(time_duration_4),
                               type = "synthpop")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_sd2011_v04_package_analyze.csv"), row.names=FALSE)

# v05 wkabdur, eduspec are last ----

ods <- SD2011
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
time_duration_5

df_time_duration <- data.frame(data = c("sd2011_v05"),
                               duration = c(time_duration_5),
                               type = "synthpop")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_sd2011_v05_package_analyze.csv"), row.names=FALSE)

# v06 - eduspec is last and wkabdur is numeric ----

ods <- SD2011
df_ods <- ods

df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods$wkabdur <- as.numeric(df_ods$wkabdur)

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_6 <- as.numeric(time_end[1] - time_start[1])
time_duration_6

df_time_duration <- data.frame(data = c("sd2011_v06"),
                               duration = c(time_duration_6),
                               type = "synthpop")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_sd2011_v06_package_analyze.csv"), row.names=FALSE)

# v07 - wkabdur, random ar last ----
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
unique_values <- sample(letters2, 40, replace = FALSE)
df_ods$random <- sample(unique_values, nrow(df_ods), replace = TRUE)

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_7 <- as.numeric(time_end[1] - time_start[1])
time_duration_7

df_time_duration <- data.frame(data = c("sd2011_v07_40"),
                               duration = c(time_duration_7_40),
                               type = "synthpop")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_sd2011_v07_40_package_analyze.csv"), row.names=FALSE)
