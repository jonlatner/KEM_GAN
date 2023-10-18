# Load necessary library
library(dplyr)

# Set the seed for reproducibility
set.seed(123)

# Create a synthetic dataset for Population 1
age_population0 <- rnorm(100, mean = 30, sd = 5)
income_population0 <- rnorm(100, mean = 50000, sd = 10000)

# Combine age and income into a data frame for Population 1
data_population0 <- data.frame(Age = age_population0, Income = income_population0)

# Create a synthetic dataset for Population 1
age_population1 <- rnorm(100, mean = 30, sd = 5)
income_population1 <- rnorm(100, mean = 50000, sd = 10000)

# Combine age and income into a data frame for Population 1
data_population1 <- data.frame(Age = age_population1, Income = income_population1)

# Create a synthetic dataset for Population 2
age_population2 <- rnorm(100, mean = 40, sd = 10)
income_population2 <- rnorm(100, mean = 70000, sd = 15000)

# Combine age and income into a data frame for Population 2
data_population2 <- data.frame(Age = age_population2, Income = income_population2)

# Combine both data frames into a list
synthetic_data_list <- list(data_population1, data_population2)

test <- compare(synthetic_data_list,data_population0,stat = "counts")

test$tables

