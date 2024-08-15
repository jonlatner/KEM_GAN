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
library(rpart.plot)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
graphs = "graphs/"
original_data = "data_files/original/"
synthpop_data = "data_files/synthetic/synthpop/"
datasynthesizer_data = "data_files/synthetic/datasynthesizer/"

setwd(main_dir)

#functions
options(scipen=999) 

# Set seed for reproducibility
my.seed = 1237
set.seed(my.seed)

# load original data ----


# Load original data ----
ods <- read.csv(paste0(original_data,"simulated.csv"))

# Graph numeric tree (disclosive) ----

# generate synthetic data
sds <- syn(ods, m = 1, seed = my.seed,models = TRUE)
model_numeric <- sds$models$var4
rpart.plot(model_numeric, type = 1,roundint = FALSE)

# Define the PDF file and open the device
pdf(paste0(graphs,"graph_tree_numeric.pdf"), height = 4, width = 10)

# Generate the plot
rpart.plot(model_numeric, type = 1)

# Close the PDF device
dev.off()

# Graph categorical tree (protective) ----

# generate synthetic data
sds <- syn(ods, m = 1, seed = my.seed,minnumlevels = 5,models = TRUE)
model_categorical <- sds$models$var4

# Define the PDF file and open the device
pdf(paste0(graphs,"graph_tree_categorical.pdf"), height = 4, width = 10)

# Generate the plot
rpart.plot(model_categorical, type = 1,roundint = FALSE)

# Close the PDF device
dev.off()

# Graph combined plots ----

# Define the PDF file and open the device
pdf(paste0(graphs,"graph_tree_combined.pdf"), height = 4, width = 10)

# Set up the plotting area to have 1 row and 2 columns
par(mfrow = c(1, 2))

# Plot the first tree
rpart.plot(model_numeric, type = 1,main = "Numeric (disclosive)",roundint = FALSE)

# Plot the second tree
rpart.plot(model_categorical, type = 1,main = "Categorical (protective)",roundint = FALSE)

par(mfrow = c(1, 1))


# Close the PDF device
dev.off()
