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

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"

setwd(main_dir)

#functions
options(scipen=999) 

# Create fake synthetic data ----

# Set a seed for reproducibility
set.seed(1234)

# Create simulated data with 5 continuous variables using a loop
n <- 5000  # Number of observations
num_continuous <- 3  # Number of continuous variables
continuous_variables <- matrix(rnorm(n * num_continuous), ncol = num_continuous)

# Create data frame with continuous variables using a loop
simulated_data <- data.frame(matrix(NA, nrow = n, ncol = num_continuous))

# Generate and add continuous variables using a loop
for (i in 1:num_continuous) {
  var_name <- paste("ContinuousVar", i, sep = "")
  simulated_data[[var_name]] <- continuous_variables[, i]
}

# Add categorical variable
combinations <- combn(LETTERS, 2, FUN = paste0, collapse = "")
letters_combinations <- sample(combinations, 25)
simulated_data$CategoricalVar1 <- factor(sample(letters_combinations, n, replace = TRUE))
simulated_data$CategoricalVar2 <- factor(sample(letters[1:3], n, replace = TRUE))
simulated_data$CategoricalVar3 <- factor(sample(letters[1:2], n, replace = TRUE))

# Identify and drop variables starting with "X"
simulated_data <- simulated_data[, !grepl("^X", names(simulated_data))]
simulated_data <- simulated_data[, !grepl("*NA", names(simulated_data))]

# add in missings ----

# Randomly add 10 missing values to the 'CategoricalVar1' variable
missing_indices <- sample(1:5000, 25)
simulated_data$CategoricalVar1[missing_indices] <- NA

# Randomly add 10 missing values to the 'CategoricalVar2' variable
missing_indices <- sample(1:5000, 10)
simulated_data$CategoricalVar2[missing_indices] <- NA

# Randomly add 10 missing values to the 'CategoricalVar3' variable
missing_indices <- sample(1:5000, 10)
# simulated_data$CategoricalVar3[missing_indices] <- NA

# Create data frame
ods <- simulated_data

summary(ods)

# Save ----
write.csv(ods, paste0(original_data,"simulated_v01.csv"), row.names = FALSE)