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
duration_folder = "duration/"

setwd(main_dir)

#functions
options(scipen=999) 

# Create fake synthetic data with 1 copy (and time duration) ----

df_ods <- read.csv(paste0(original_data,"SD2011.csv"))
df_ods[df_ods == ""] <- NA
df_ods <- df_ods %>%
  mutate_if(is.character, as.factor)
# Count the number of levels for each column
num_levels <- sapply(df_ods, function(x) length(levels(x)))

# Reorder columns based on the number of levels
df_ods <- df_ods[, order(-num_levels)]

# Save
write.csv(df_ods, paste0(original_data,"sd2011_ordered.csv"), row.names = FALSE)

maximum_factors <- c(10,17,18,30)
for (f in maximum_factors) {
  
  # Identify columns with categorical variables
  categorical_cols <- sapply(df_ods, function(x) is.factor(x) | is.character(x))

  # Filter out categorical columns with more than m unique values
  filtered_cols <- sapply(df_ods[categorical_cols], function(col) length(unique(col)) <= f)
  
  # Identify columns with numeric (continuous) variables
  numeric_cols <- sapply(df_ods, is.numeric)
  
  # Get column names to keep (categorical and numeric with conditions)
  cols_to_keep <- names(df_ods)[categorical_cols][filtered_cols]
  cols_to_keep <- c(cols_to_keep, names(df_ods)[numeric_cols])

  # Create a new dataframe with filtered columns
  new_df <- df_ods[, cols_to_keep]

  df_duration <- data.frame()
  
  # Loop to create subsets with an increasing number of variables
  columns <- names(new_df)
  for (i in 2:length(columns)) {
    subset <- new_df %>% select(1:i)  # Subset with the first i columns
    
    # start clock
    time_start <- proc.time()
    
    df_synds <- syn(subset, m = 1)
    
    # end clock
    time_end <- proc.time()
    time_duration <- as.numeric(time_end[1] - time_start[1])
    
    duration = data.frame(type="synthpop",
                          columns=i,
                          total_columns=length(columns),
                          max_factors=f,
                          duration=time_duration)
    
    df_duration <- rbind(df_duration,duration)
  }    
  write.csv(df_duration, file = paste0(duration_folder,"duration_synthpop_data_SD2011_factors_",f,".csv"), row.names = FALSE)
  df_duration
}

