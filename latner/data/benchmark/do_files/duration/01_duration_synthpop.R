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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
duration_folder = "duration/SD2011/"

setwd(main_dir)

#functions
options(scipen=999) 

# Open SD2011 ----

df_ods <- read.csv(paste0(original_data,"SD2011.csv"))
df_ods[df_ods == ""] <- NA
df_ods <- df_ods %>%
  mutate_if(is.character, as.factor)

# Select variables ----

df_new <- df_ods %>%
  select(age,eduspec,sex,alcabuse)

# Run synthesizer (with missing values) ---- 

df_duration <- data.frame()

time_start <- proc.time()
df_synds <- syn(df_new, m = 1)
time_end <- proc.time()
time_duration <- as.numeric(time_end[1] - time_start[1])

# save RDS file for future use with synthpop package (i.e. utility measures)
saveRDS(df_synds, paste0(duration_folder,"SD2011_duration_w_missing.rds"))

duration = data.frame(type="synthpop",
                         data="SD2011",
                      missing = "yes",
                         duration=time_duration)
df_duration <- rbind(df_duration,duration)

# Run synthesizer (without missing values) ---- 

df_new <- na.omit(df_new)

time_start <- proc.time()
df_synds <- syn(df_new, m = 1)
time_end <- proc.time()
time_duration <- as.numeric(time_end[1] - time_start[1])

# save RDS file for future use with synthpop package (i.e. utility measures)
saveRDS(df_synds, paste0(duration_folder,"SD2011_duration_wo_missing.rds"))

duration = data.frame(type="synthpop",
                      data="SD2011",
                      missing = "no",
                      duration=time_duration)
df_duration <- rbind(df_duration,duration)

# save duration ----
df_duration

write.csv(df_duration, file = paste0(duration_folder,"duration_synthpop_data.csv"), row.names = FALSE)
