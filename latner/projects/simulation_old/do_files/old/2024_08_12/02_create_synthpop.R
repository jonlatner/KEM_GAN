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
synthetic_data = "data_files/synthetic/synthpop/"

setwd(main_dir)

#functions
options(scipen=999) 


roc_univariate <- function(original, synthetic, var_num) {
  # create frequency tables for the original and synthetic data, on the variable
  orig_table <- as.data.frame(ftable(original[,var_num]))
  syn_table <- as.data.frame(ftable(synthetic[,var_num]))
  # calculate the proportions by dividing by the number of records in each dataset
  orig_table$prop <- orig_table$Freq/nrow(original)
  syn_table$prop <- syn_table$Freq/nrow(synthetic)
  # merge the two tables, by the variable
  combined<- merge(orig_table, syn_table, by= c('Var1'), all = TRUE) 
  # merging will induce NAs where there is a category mismatch - i.e. the category exists in one dataset but not the other
  # to deal with this set the NA values to zero:
  combined[is.na(combined)] <- 0
  # get the maximum proportion for each category level:
  combined$max <- pmax(combined$prop.x, combined$prop.y)
  # get the minimum proportion for each category level:
  combined$min <- pmin(combined$prop.x, combined$prop.y)
  # roc is min divided by max (a zero value for min results in a zero for ROC, as expected)
  combined$roc <- combined$min/combined$max 
  combined$roc[is.na(combined$roc)] <- 1
  return(mean(combined$roc))
}

apply_roc_univariate <- function(original, synthetic) {
  # Get the column names
  columns <- colnames(original)
  
  # Apply the roc_univariate function to each column
  roc_results <- lapply(columns, function(col) {
    roc_univariate(original, synthetic, col)
  })
  
  # Convert the results to a named vector
  names(roc_results) <- columns
  return(roc_results)
}

# Set seed for reproducibility
my.seed = 1234
set.seed(my.seed)

# Load original data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))
# df_ods <- df_ods %>%
#   mutate(across(where(is.integer), as.character))

# Create fake synthetic data with m copies ----

copies <- c(10)
df_frequency <- data.frame()
df_roc_values <- data.frame()
for (c in 1:copies) {
  my.seed = my.seed + 1
  sds <- syn(df_ods, m = 1, seed = my.seed)
  sds <- sds$syn

  # Create a ROC table
  # Apply the function to all variables in the data frame
  roc_values <- apply_roc_univariate(df_ods, sds)
  roc_values <- data.frame(roc_values)%>%
    mutate(number = 1) %>%
    pivot_longer(!number) %>%
    mutate(number = c)
  df_roc_values <- rbind(df_roc_values,roc_values)
  
  # Create a frequency table
  sds$combine <- paste(sds$var1, sds$var2, sds$var3, sds$var4, sep = "")
  sds <- sds %>%
    select(-matches("var"))
  frequency <- as.data.frame(table(sds))
  frequency$number <- c
  df_frequency <- rbind(df_frequency,frequency)
}


# Save ----

write.csv(df_frequency,paste0(synthetic_data,"synthpop_frequency.csv"), row.names = FALSE)
write.csv(df_roc_values,paste0(synthetic_data,"synthpop_roc_values.csv"), row.names = FALSE)

