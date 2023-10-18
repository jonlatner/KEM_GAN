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
library(broom)
library(tidyverse)
library(ggh4x) # facet_nested
library(synthpop)
library(fastDummies) # dummy_cols
library(mice)

# FOLDERS - ADAPT THIS PATHWAY
# main_dir = "N:/Ablagen/D01700-KEM/Latner/little_etal_2021/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/simulation_cat/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/ctgan/"
tables = "tables/ctgan/"

# Load data ----

# original data

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))
summary(df_ods)

df <- df_ods
# Loop through each column in the data frame
for (col in names(df)) {
  # Check if the column is categorical (factor or character)
  if (is.character(df[[col]]) || is.factor(df[[col]])) {
    # Replace "" with NA
    df[[col]] <- ifelse(df[[col]] == "", NA, df[[col]])
    # Get the frequency table of the column, after omitting missing values
    freq_table <- table(na.omit(df[[col]]))
    # Get the value with the highest frequency
    max_freq_value <- names(freq_table)[which.max(freq_table)]
    # Create a new dichotomous variable based on the highest frequency value
    new_col <- ifelse(df[[col]] == max_freq_value, 1, 0)
    # Reinsert missing values
    new_col <- ifelse(df[[col]] == "", NA, new_col)
    # Drop original variable
    df[[col]] <- NULL
    # Assign the new dichotomous variable to the data frame
    df[[paste(col,max_freq_value,sep = "_")]] <- new_col
  }
}
df_ods <- df
head(df_ods, 10)
rm(df)

# synthetic data

epochs = c(900)
discriminator = c(10)
batch = c(1000)
frequency = c("False")
copies = c(5)
for (e in epochs) {
  for (d in discriminator) {
    for (b in batch) {
      for (f in frequency) {
        for (m in copies) {
          df_sds =list()
          for (j in 1:m) {
            sds <- read.csv(paste0(synthetic_data,"sds_ctgan_tuning_e_",e,"_d_",d,"_b_",b,"_f_",f,"_m_",m,"_n_",j,".csv"))
            sds <- sds %>% 
              mutate_if(is.character, factor)
            
            # Get the names of the categorical variables
            vars_categorical <- names(sds)[sapply(sds, function(x) is.character(x) || is.factor(x))]

            # Get the names of the continuous variables
            vars_continuous <- names(sds)[sapply(sds, function(x) !is.character(x) && !is.factor(x))]
            
            # Transform character and factor variables into dummy variables
            sds_dummies <- dummy_cols(sds, select_columns = vars_categorical, remove_selected_columns = TRUE)
            
            # Select only columns that are in df_ods
            sds_dummies <- sds_dummies[, names(df_ods)]
            
            df_sds[[j]] <- sds_dummies
          }
        }
      }
    }
  }
}

dv <- "var_1_B"
iv <- "var_2_B + var_3_A + var_4_A"
fit_1 <- with(data = df_sds[[1]], exp = lm(var_1_B ~ var_2_B + var_3_A + var_4_A))

model <- lm(formula = formula(paste(dv, "~ .")), data = df_sds[[1]])

fit_2 <- with(data = df_sds[[2]], exp = lm(var_1_B ~ var_2_B + var_3_A + var_4_A))
summary(pool.syn(list(model,fit_2), rule = "reiter2003"))

fit_ods <- summary(lm(var_1_B ~ var_2_B + var_3_A + var_4_A, data = df_ods))

