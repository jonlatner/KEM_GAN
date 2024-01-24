# Top commands ----

# Create empty R application (no figures, data frames, packages, etc.)
# Get a list of all loaded packages
packages <- search()[grepl("package:", search())]
# Unload each package
for (package in packages) {
  unloadNamespace(package)
}

rm(list=ls(all=TRUE))

library(ggplot2)
library(tidyverse)
library(synthpop)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/"
graphs = "graphs/"
tables = "tables/"

#functions
options(scipen=999) 

# Create simulated, correlated data ----


df_ods <- SD2011
df_ods <- select(df_ods,sex,age,income,edu)
# df_ods$income <- log(df_ods$income)
model <- lm(log(income)~sex + age + edu, data = df_ods)
sd(model$residuals)
summary(model)

# Create simulated, correlated data ----

# Set seed for reproducibility
set.seed(1234)

# Set the number of observations
n_obs <- 5000

# Define the true coefficients
beta_age <- 0.003  # Replace with your actual coefficient
beta_educationM <- .4  # Replace with your actual coefficient
beta_educationH <- .8  # Replace with your actual coefficient
beta_genderF <- -.3  # Replace with your actual coefficient
intercept <- 6.36  # Replace with your actual intercept

# Create a data frame with the variables

# Generate age data
age <- rnorm(n_obs, mean = 48, sd = 18.5)
age <- pmax(pmin(age, 96), 16)
summary(age)

# Generate country data
country <- factor(sample(letters[1:26], n_obs, replace = TRUE))
# country_dummy <- model.matrix(~ country - 1)  # Create dummy variables for education

# Generate education data
education <- sample(c("L", "M", "H"), n_obs, replace = TRUE, prob = c(0.5, 0.3, 0.2))
education_dummy <- model.matrix(~ education - 1)  # Create dummy variables for education

# Generate gender data
gender <- sample(c("M", "F"), n_obs, replace = TRUE, prob = c(0.45, 0.55))
gender_dummy <- model.matrix(~ gender - 1)  # Create dummy variables for gender

# Combine variables into a data frame
sim_data <- data.frame(age, education_dummy, gender_dummy,country)

# Create the dependent variable (income) based on the true coefficients
income <- intercept + 
  beta_age * sim_data$age +
  sim_data$educationM * beta_educationM +
  sim_data$educationH * beta_educationH +
  sim_data$genderF * beta_genderF +
  rnorm(n_obs, mean = 0, sd = .75)  # Replace with your desired error term standard deviation

# Combine variables into a data frame
sim_data <- data.frame(income, age, education, gender, country)
sim_data <- sim_data %>%
  mutate_if(is.character, as.factor)

summary(sim_data)
sim_model <- lm(income~gender + age + education, data = sim_data)
summary(sim_model)
summary(model)

# Randomly add 10 missing values to the 'CategoricalVar1' variable
missing_indices <- sample(1:5000, 25)
sim_data$country[missing_indices] <- NA

# Randomly add 10 missing values to the 'CategoricalVar2' variable
missing_indices <- sample(1:5000, 10)
sim_data$gender[missing_indices] <- NA

sim_data$educationL <- NULL
sim_data$genderM <- NULL

summary(sim_data)
summary(df_ods)

df_synds <- syn(df_ods, m = 1)
df_synds <- syn(sim_data, m = 1)
