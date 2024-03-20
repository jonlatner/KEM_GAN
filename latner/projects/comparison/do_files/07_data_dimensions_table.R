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
library(xtable)
library(ggcorrplot)
library(fastDummies)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
tables = "tables/"

setwd(main_dir)

#functions
options(scipen=999) 

# Create fake synthetic data ----

ods <- SD2011
ods[ods == ""] <- NA
ods$wkabdur <- as.numeric(ods$wkabdur)

summary(ods)


# Function to generate a summary for a data frame
summary_df <- function(data) {
  summary_matrix <- matrix(NA, 
                           nrow = ncol(data), 
                           ncol = 5,
                           dimnames = list(names(data), c("Type","Observations","Unique Values", "Missings","Negative values")))
  
  for (col in names(data)) {
    summary_matrix[col, "Type"] <- class(data[[col]])
    summary_matrix[col, "Observations"] <- length(data[[col]])
    summary_matrix[col, "Unique Values"] <- length(unique(data[[col]]))
    summary_matrix[col, "Missings"] <- sum(is.na(data[[col]]))
    summary_matrix[col, "Negative values"] <- sum(data[[col]]<0,na.rm = TRUE)
  }
  
  summary_df <- as.data.frame(summary_matrix)
  return(summary_df)
}

# Use the function with simulated data
df_data_structure <- data.frame(summary_df(ods))
df_data_structure$Variable <- rownames(df_data_structure) 
rownames(df_data_structure) <- NULL  # Remove row names
df_data_structure$Description = c(
  "Sex", 
  "Age of person, 2011", 
  "Age group, 2011", 
  "Category of the place of residence",
  "Region (voivodeship)", 
  "Highest educational qualification, 2011", 
  "Discipline of completed qualification",
  "Socio-economic status, 2011", 
  "Total duration of unemployment in the last 2 years (in months)",
  "Personal monthly net income", 
  "Marital status",
  "Month of marriage", 
  "Year of marriage",
  "Month of separation/divorce", 
  "Year of separation/divorce",
  "Perception of life as a whole", 
  "Depression symptoms indicator",
  "View on interpersonal trust", 
  "Trust in own family members",
  "Trust in neighbours", 
  "Active engagement in some form of sport or exercise",
  "Number of friends", 
  "Smoking cigarettes",
  "Number of cigarettes smoked per day", 
  "Drinking too much alcohol",
  "Starting to use alcohol to cope with troubles", 
  "Working abroad in 2007-2011 (in months)",
  "Total time spent on working abroad", 
  "Plans to go abroad to work in the next two years",
  "Intended duration of working abroad", 
  "Intended destination country",
  "Knowledge of English language", 
  "Height of person",
  "Weight of person", 
  "Body mass index (weight - kg/(height - cm$^2$)*10000)"
  )

df_data_structure <- df_data_structure %>%
  mutate(Number = row_number()) %>%
  select(Number, Variable,Description,everything()) %>%
  mutate(Negative.values = ifelse(Type == "factor", yes = 0, no = Negative.values),
         Generated = ifelse((Variable == "bmi" | Variable == "agegr"), yes = "Yes", no = NA),
         Quirks = ifelse((Variable == "nociga" | Variable == "agegr" | Variable == "bmi" | Variable == "wkabdur"), yes = "Yes", no = NA)) 

print(df_data_structure)

# Print the data frame as a LaTeX table using xtable
latex_table <- xtable(df_data_structure)
print.xtable(latex_table, 
             include.rownames = FALSE, 
             sanitize.text.function = identity,
             floating = FALSE,
             booktabs = TRUE, 
             file = paste0(tables,"table_sd2011_data_structure.tex"))

rm(latex_table,df_data_structure)

# Count the number of factor variables
num_factor_variables <- sum(sapply(ods, is.factor))

# Count the number of numeric variables
num_numeric_variables <- sum(sapply(ods, is.numeric))

# Print the results
cat("Number of factor variables:", num_factor_variables, "\n")
cat("Number of numeric variables:", num_numeric_variables, "\n")

# Quirky variables

# BMI (1 individual with bmi, but missing weight)
df_bmi_missing <- ods %>%
  select(height, weight, bmi) %>%
  filter(is.na(height) | is.na(weight))
df_bmi_missing %>% filter(!is.na(bmi))

# Age group
with(ods,table(agegr,age,useNA = "ifany"))

# Smoking / number of cigaretes per day
with(ods,table(smoke,nociga))

