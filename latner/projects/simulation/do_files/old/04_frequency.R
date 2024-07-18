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

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
synthpop_data = "data_files/synthetic/synthpop/"
datasynthesizer_data = "data_files/synthetic/datasynthesizer/"

setwd(main_dir)

#functions
options(scipen=999) 

# load original data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))
df_ods$combine <- paste(df_ods$var1, df_ods$var2, df_ods$var3, df_ods$var4, sep = "")
df_ods <- df_ods[5]

copies <- c(1)
data <- c("simulated")

# Load synthetic data from synthpop ----

df_sds <- data.frame()
for (d in data) {
  for (c in copies) {
    for (j in 1:c) {
      sds <- read.csv(paste0(synthpop_data,"sds_synthpop_",d,"_m_",c,"_n_",j,".csv"))
      df_sds <- rbind(df_sds,sds)
    }
  }
  df_sds$combine <- paste(df_sds$var1, df_sds$var2, df_sds$var3, df_sds$var4, sep = "")
  df_sds <- df_sds[5]
}

df_sds_synthpop <- df_sds

# Load synthetic data from datasynthesizer ----

sds_datasynthesizer <- c()
privacy = c(0,0.1,0.25,0.5,0.75,1,10,25,50)
for (d in data) {
  for (e in privacy) {
    df_sds <- data.frame()
    for (c in copies) {
      for (j in 1:c) {
        sds <- read.csv(paste0(datasynthesizer_data,"sds_datasynthesizer_",d,"_e_",e,"_m_",c,"_n_",j,".csv"))
        df_sds <- rbind(df_sds,sds)
      }
    }
    sds_datasynthesizer <- c(sds_datasynthesizer,paste0("df_sds_datasynthesizer_",e))
    df_sds$combine <- paste(df_sds$var1, df_sds$var2, df_sds$var3, df_sds$var4, sep = "")
    df_sds <- df_sds[5]
    assign(paste0("df_sds_datasynthesizer_",e),df_sds)
  }
}

# frequency ----

sds = c("df_sds_synthpop",sds_datasynthesizer)
for (s in sds) {
  df_sds <- get(s)
  print(paste0(s,": ", roc_univariate(df_ods,df_sds,1)))
}
