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
ods <- select(ods, age, edu)
ods$bmi <- NULL
ods$agegr <- NULL
ods[ods < 0] <- NA
ods[ods == ""] <- NA

# v08 - wkabdur, random ar last ----

# Specify the number of additional random variables you want to add
num_additional_variables <- c(1,2,3)  # Change this according to your needs
num_values <- c(20, 25, 30)  # Change this according to your needs

# Create new data frames with an increasing number of random variables
for (i in num_additional_variables) {
  for (v in num_values) {
  
  # Generate new random variable names
  new_variable_names <- paste0("random_", 1:i)
  
  # Add new random variables to the original data frame
  unique_values <- sample(letters2, v, replace = FALSE)
  additional_variables <- replicate(i, sample(unique_values, nrow(ods), replace = TRUE))
  new_df <- cbind(ods, setNames(as.data.frame(additional_variables), new_variable_names))
  
  # Print or use the new data frame as needed
  print(summary(new_df))
  # print(table(new_df$random_1))
  
  # Optionally, you can save each new data frame to a list or a file if needed
  # new_data_frames_list[[i]] <- new_df
  # write.csv(new_df, file = paste0("new_df_", i, ".csv"))
  }
}
