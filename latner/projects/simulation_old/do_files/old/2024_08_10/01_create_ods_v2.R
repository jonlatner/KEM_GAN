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
library(ggh4x) # facet_nested

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"

setwd(main_dir)

#functions
options(scipen=999) 

# Set seed for reproducibility
my.seed = 1234
set.seed(my.seed)

# Create simulated data ----

# Define the number of samples
n <- 1000

# Define the possible combinations of y
combinations <- expand.grid(y1 = c(0, 1), y2 = c(0, 1), y3 = c(0, 1), y4 = c(0, 1))

# Define C-16 and c16
C_minus_16 <- combinations[1:15, ]
c16 <- combinations[16, ]

# Initialize an empty matrix to store the results
y <- matrix(0, nrow = n, ncol = 4)

# Generate the first n-1 samples from the multinomial distribution
for (i in 1:(n-1)) {
  # Sample an index from 1 to 15 with equal probability
  sampled_index <- sample(1:15, 1)
  y[i, ] <- as.numeric(C_minus_16[sampled_index, ])
}

# Set the last sample to c16
y[n, ] <- as.numeric(c16)

# Convert the matrix to a data frame
y_df <- as.data.frame(y)
colnames(y_df) <- c("var1", "var2", "var3", "var4")

# Calculate the correlation matrix
cor_matrix <- cor(y_df)

# Print the correlation matrix
print(cor_matrix)

