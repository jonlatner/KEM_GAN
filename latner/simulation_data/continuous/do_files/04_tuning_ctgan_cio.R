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

# FOLDERS - ADAPT THIS PATHWAY
# main_dir = "N:/Ablagen/D01700-KEM/Latner/little_etal_2021/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/simulation_data/continuous/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/ctgan/"
tables = "tables/ctgan/"

#functions
source("/Users/jonathanlatner/Google Drive/My Drive/IAB/little_etal_2021/do_files/R/cio_function_multiple_v2.R")
source("/Users/jonathanlatner/Google Drive/My Drive/IAB/little_etal_2021/do_files/R/cio_function.R")
options(scipen=999) 

# Load data ----

# original data

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))

# Apply log transformation to continuous variables
original <- df_ods %>%
  mutate_if(.predicate = is.numeric, .funs = ~log(.))

# Get the names of the categorical variables
vars_categorical <- names(df_ods)[sapply(df_ods, function(x) is.character(x) || is.factor(x))]
# Get the names of the continuous variables
vars_continuous <- names(df_ods)[sapply(df_ods, function(x) !is.character(x) && !is.factor(x))]
vars_continuous

# synthetic data
df_results_sds <- data.frame()
df_results_ods <- data.frame()
df_output_cio <- data.frame()

epochs = c(300, 600, 900)
discriminator = c(1, 5, 10)
batch = c(500, 1000)
frequency = c("True", "False")
copies = c(5, 10)
for (e in epochs) {
  for (d in discriminator) {
    for (b in batch) {
      for (f in frequency) {
        for (m in copies) {
          # Loop through each continuous variables as dependent variable
          synthetic <- syn(df_ods, m = m)
          
          for (dep_var in vars_continuous) {
            df_sds <- list()
            for (j in 1:m) {
              sds <- read.csv(paste0(synthetic_data,"sds_ctgan_tuning_e_",e,"_d_",d,"_b_",b,"_f_",f,"_m_",m,"_n_",j,".csv"))

              # Apply log transformation to continuous variables
              sds <- sds %>%
                mutate_if(.predicate = is.numeric, .funs = ~log(.))
              
              # Subset the data frame to include only the current dependent variable
              ind_vars <- c(vars_categorical, vars_continuous)
              subset_ods <- original[, c(dep_var, setdiff(ind_vars, dep_var))]
              subset_sds <- sds[, c(dep_var, setdiff(ind_vars, dep_var))]
              
              # remove missing values
              subset_ods<-subset_ods[complete.cases(subset_ods),]
              subset_sds<-subset_sds[complete.cases(subset_sds),]
              
              # Replace synthetic data frame with actual synthetic data from package
              synthetic$syn[[j]] <- subset_sds
            }
            
            model_sds <- lm.synds(income ~ ., data = synthetic)
            sds_output <- summary(model_sds)
            
            test <- compare(model_sds, df_ods)
            test$mean.ci.overlap
            test$mean.abs.std.diff
            
            output_sds <- data.frame(term = row.names(sds_output$coefficients),
                               estimate = sds_output$coefficients[,1],
                               std.error = sds_output$coefficients[,2],
                               statistic = sds_output$coefficients[,3],
                               p.value = sds_output$coefficients[,4])
            results_sds <- output_sds
            results_sds$dep_var <- dep_var
            results_sds$data <- "sds"
            results_sds$epochs <- e
            results_sds$discriminator <- d
            results_sds$batch <- b
            results_sds$frequency <- f
            results_sds$copies <- m
            results_sds$df<-NULL
            df_results_sds <- rbind(df_results_sds, results_sds)
            
            model_ods <- lm(paste(dep_var, "~ ."), data = subset_ods)
            ods_output <- tidy(model_ods)
            results_ods <- ods_output
            results_ods$dep_var <- dep_var
            results_ods$data <- "ods"
            results_ods$epochs <- e
            results_ods$discriminator <- d
            results_ods$batch <- b
            results_ods$frequency <- f
            results_ods$copies <- m
            df_results_ods <- rbind(df_results_ods, results_ods)
            
            output <- CIO_function_multiple(ods_output,output_sds)
            output$dep_var <- dep_var
            output$epochs <- e
            output$discriminator <- d
            output$batch <- b
            output$frequency <- f
            output$copies <- m
            df_output_cio <- rbind(df_output_cio,output)
          }
        }
      }
    }
  }
}

df_results_1 <- rbind(df_results_ods,df_results_sds)

df_output_cio_1 <- df_output_cio
