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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/"

original_data_benchmark = "benchmark/data_files/original/"
original_data_categorical = "categorical_dim/data_files/original/"
original_data_continuous = "continuous_dim/data_files/original/"
tables = "benchmark/tables/"

setwd(main_dir)

# Load benchmark data ----

data <- c("adult","grid","gridr","sd2011_small","sd2011")
for (d in data) {
  df_ods <- read.csv(paste0(original_data_benchmark,d,".csv"))
  assign(d,df_ods)
}
rm(df_ods)

# Summarize dimensions ----

# List of all objects in the environment
all_objects <- ls()

# Filter to get only the names of data frames
data_frame_names <- all_objects[sapply(all_objects, function(x) is.data.frame(get(x)))]

# Create a list of data frames with names
list_of_data_frames <- mget(data_frame_names)

# Now list_of_data_frames is a named list of all data frames in the environment
names(list_of_data_frames)

# Function to count numeric and non-numeric variables
count_var_types <- function(df) {
  num_vars <- sum(sapply(df, is.numeric))
  non_num_vars <- ncol(df) - num_vars
  return(c(num_vars, non_num_vars))
}

# Initialize an empty data frame to store results
results <- data.frame(DataFrameName=character(),
                      Rows=integer(),
                      Columns=integer(),
                      NumericVars=integer(),
                      NonNumericVars=integer(),
                      stringsAsFactors=FALSE)


# Loop over each data frame
for (df_name in names(list_of_data_frames)) {
  df <- list_of_data_frames[[df_name]]
  rows <- nrow(df)
  cols <- ncol(df)
  var_counts <- count_var_types(df)
  
  # Append to results table
  results <- rbind(results, data.frame(Data=df_name,
                                       Rows=rows,
                                       Columns=cols,
                                       NumericVars=var_counts[1],
                                       NonNumericVars=var_counts[2]))
}

# Display the results
print(results)

# Latex table
latex_table <- xtable(results)
print.xtable(latex_table, 
             include.rownames = FALSE, 
             floating = FALSE,
             booktabs = TRUE, 
             file = paste0(tables,"data_dimensions_benchmark.tex"))


