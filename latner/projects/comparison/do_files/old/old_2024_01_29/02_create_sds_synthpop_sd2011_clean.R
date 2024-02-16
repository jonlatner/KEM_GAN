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
library(rpart)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
duration = "duration/"

setwd(main_dir)

#functions
options(scipen=999) 

# Create fake synthetic data with 1 copy (and time duration) ----

copies <- c(1)
data <- c("sd2011_clean")
for (d in data) {
  print(d)
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  # df_ods[df_ods == ""] <- NA
  # df_ods[df_ods < 0] <- NA
  # df_ods <- df_ods %>%
  #   mutate_if(is.character, as.factor)
  
  for (c in copies) {
    
    # start clock
    time_start <- proc.time()
    
    df_synds <- syn(df_ods, m = c)
    
    # end clock
    time_end <- proc.time()
    time_duration <- as.numeric(time_end[1] - time_start[1])
    
    df_duration = data.frame(type="synthpop",
                             data=d,
                             duration=time_duration)
    write.csv(df_duration, file = paste0(duration,"duration_synthpop_data_",d,".csv"), row.names = FALSE)
    

    # save RDS file for future use with synthpop package (i.e. utility measures)
    saveRDS(df_synds, paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
    
    # save csv file
    for (j in 1:c) {
      synthpop_df <- df_synds$syn
      write.csv(synthpop_df, file = paste0(synthetic_data,"sds_synthpop_",d,"_m_",c,"_n_",j,".csv"), row.names = FALSE)
    }
  }
}

# Create fake synthetic data with m copies ----

copies <- c(5)
for (d in data) {
  print(d)
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  # df_ods[df_ods == ""] <- NA
  # df_ods[df_ods < 0] <- NA
  # df_ods <- df_ods %>%
  #   mutate_if(is.character, as.factor)

  for (c in copies) {

    df_synds <- syn(df_ods, m = c)

    # save RDS file for future use with synthpop package (i.e. utility measures)
    saveRDS(df_synds, paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))

    # save csv file
    for (j in 1:c) {
      synthpop_df <- df_synds$syn[j]
      write.csv(synthpop_df, file = paste0(synthetic_data,"sds_synthpop_",d,"_m_",c,"_n_",j,".csv"), row.names = FALSE)
    }
  }
}

