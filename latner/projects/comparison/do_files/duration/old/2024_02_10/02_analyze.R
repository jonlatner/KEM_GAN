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
library(tidyverse)
library(synthpop)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/ctgan/"
tables = "tables/ctgan/"
duration = "duration/"

setwd(main_dir)

# CTGAN/datasynthesizer ----

data <- c("sd2011_v00","sd2011_v01","sd2011_v02","sd2011_v03","sd2011_v04","sd2011_v05","sd2011_v06","sd2011_v07")
sdg <- c("ctgan","datasynthesizer")

df_duration <- data.frame()
for (d in data) {
  for (s in sdg) {
    data <- read.csv(paste0(duration,"duration_",s,"_",d,".csv")) %>%
      rename(sdg=type)
    df_duration <- rbind(df_duration,data)
  }
}
df_duration

# synthpop ----

synthpop_csv <- read.csv(paste0(duration,"duration_synthpop_sd2011_csv_analyze.csv")) %>%
  rename(sdg=type) %>%
  mutate(sdg="synthpop (csv)")
synthpop_package <- read.csv(paste0(duration,"duration_synthpop_sd2011_package_analyze.csv")) %>%
  rename(sdg=type) %>%
  mutate(sdg="synthpop (package)")

df_synthpop <- rbind(synthpop_csv,synthpop_package)
df_synthpop

# combine ----

df_duration <- rbind(df_duration,df_synthpop) %>%
  arrange(sdg,data)
df_duration

# Graph ----
