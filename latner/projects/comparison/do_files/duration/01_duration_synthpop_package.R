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

# Load original data ----

ods <- SD2011
ods$bmi <- NULL
ods$agegr <- NULL
ods[ods < 0] <- NA
ods[ods == ""] <- NA

# baseline ----

df_ods <- ods
time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_0 <- as.numeric(time_end[1] - time_start[1])
time_duration_0

df_time_duration <- data.frame(data = c("sd2011_v00"),
                               duration = c(time_duration_0),
                               type = "synthpop (package)")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_package_sd2011_v00.csv"), row.names=FALSE)

# v01 - without eduspec or wkabdur ----


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
                               type = "synthpop (package)")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_package_sd2011_v01.csv"), row.names=FALSE)

# v02 - eduspec is last and without wkabdur ----


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
                               type = "synthpop (package)")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_package_sd2011_v02.csv"), row.names=FALSE)

# v03 - wkabdur is last and without eduspec ----


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
                               type = "synthpop (package)")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_package_sd2011_v03.csv"), row.names=FALSE)

# v04 eduspec, wkabdur  are last ----


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
                               type = "synthpop (package)")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_package_sd2011_v04.csv"), row.names=FALSE)

# v05 wkabdur, eduspec are last ----


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
                               type = "synthpop (package)")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_package_sd2011_v05.csv"), row.names=FALSE)

# v06 - eduspec is last and wkabdur is numeric ----


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
                               type = "synthpop (package)")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_package_sd2011_v06.csv"), row.names=FALSE)

# v07 - wkabdur, random ar last ----
# random has XX values


df_ods <- ods

df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods$wkabdur <- as.numeric(df_ods$wkabdur)

# Set seed for reproducibility
set.seed(123)

# Create a random variable with 5000 cases that XX unique character values
unique_values <- sample(letters2, 35, replace = FALSE)
df_ods$random <- sample(unique_values, nrow(df_ods), replace = TRUE)

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration_7_35 <- as.numeric(time_end[1] - time_start[1])
time_duration_7_35

df_time_duration <- data.frame(data = c("sd2011_v07_40"),
                               duration = c(time_duration_7_35),
                               type = "synthpop (package)")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_package_sd2011_v07_35.csv"), row.names=FALSE)

# v07 - wkabdur, random ar last ----
# random has XX values


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
time_duration_7_40 <- as.numeric(time_end[1] - time_start[1])
time_duration_7_40

df_time_duration <- data.frame(data = c("sd2011_v07_40"),
                               duration = c(time_duration_7_40),
                               type = "synthpop (package)")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_package_sd2011_v07_40.csv"), row.names=FALSE)

# v08 - wkabdur, random ar last ----
# random has XX values


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
                               type = "synthpop (package)")

write.csv(df_time_duration, paste0(duration,"duration_synthpop_package_sd2011_v08.csv"), row.names=FALSE)
