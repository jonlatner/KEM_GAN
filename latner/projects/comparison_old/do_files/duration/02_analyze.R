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
library(xtable)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"
duration = "duration/analyze/"

setwd(main_dir)

# CTGAN/datasynthesizer ----

data <- c("sd2011_v00","sd2011_v01","sd2011_v02","sd2011_v03","sd2011_v04","sd2011_v05","sd2011_v06","sd2011_v07_35","sd2011_v07_40","sd2011_v08")
sdg <- c("ctgan","datasynthesizer","synthpop","synthpop_package")

df_duration <- data.frame()
for (d in data) {
  for (s in sdg) {
    data <- read.csv(paste0(duration,"duration_",s,"_",d,".csv")) %>%
      rename(sdg=type)
    df_duration <- rbind(df_duration,data)
  }
}


# combine ----

df_duration %>%
  arrange(data,sdg)
df_duration

# clean ----

# rotate 
df_wide <- df_duration %>%
  pivot_wider(names_from = "sdg",values_from = "duration") %>%
  rename(version=data)

df_wide$version <- gsub("sd2011_", "", df_wide$version)
df_wide

df_wide$description <- c("Raw (SD2011)",
                         "Without eduspec or wkabdur",
                         "Without wkabdur",
                         "Without eduspec",
                         "Last variables: eduspec-wkabdur",
                         "Last variables: wkabdur-eduspec",
                         "as.numeric(wkabdur) and last variable: eduspec",
                         "v06 + Random variable (35 values)",
                         "v06 + Random variable (40 values)",
                         "v06 + 2 random variables (25 values)"
) 

df_wide <- df_wide %>%
  select(version, description, everything())
df_wide

# table ----


# Print the data frame as a LaTeX table using xtable
latex_table <- xtable(df_wide)
latex_table$description <- gsub("_", "\\_", latex_table$description)
print.xtable(latex_table, 
             include.rownames = FALSE, 
             floating = FALSE,
             booktabs = TRUE, 
             file = paste0(tables,"table_sd2011_duration.tex"))
