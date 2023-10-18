# Top commands ----
# https://alfurka.github.io/2023-01-30-creating-synthetic-values-with-synthepop-cart/
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
library(synthpop)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "N:/Ablagen/D01700-KEM/Latner/simulation/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/simulation_cat/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
graphs = "graphs/synthpop/"
tables = "tables/synthpop/"

setwd(main_dir)

# Load data ----

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))

my.seed = 1233

# Synthesize data grid ----
# Experiments were performed using the network degree and differential privacy (epsilon, ϵ) parameters.

cp_grid <- c(0.00000001, 0.000001, 0.0001, 0.01)
minbucket_grid <- c(5, 10, 25, 50)
copies <- c(5, 10)

for (a in cp_grid) {
  for (b in minbucket_grid) {
    for (c in copies) {
      my.seed <- my.seed + 1
      sds.default <- syn(df_ods,
                         cart.cp=a,
                         cart.minbucket = b,
                         m = c,
                         seed = my.seed)
      for (j in 1:c) {
        synthpop_df <- sds.default$syn[j]
        write.csv(synthpop_df, file = paste0(synthetic_data,"sds_synthpop_tuning_cp_",a,"_b_",b,"_m_",c,"_n_",j,".csv"), row.names = FALSE)
      }
    }
  }
}

copies <- c(1)
for (a in cp_grid) {
  for (b in minbucket_grid) {
    for (c in copies) {
      my.seed <- my.seed + 1
      sds.default <- syn(df_ods,
                         cart.cp=a,
                         cart.minbucket = b,
                         m = c,
                         seed = my.seed)
      synthpop_df <- sds.default$syn
      write.csv(synthpop_df, file = paste0(synthetic_data,"sds_synthpop_tuning_cp_",a,"_b_",b,"_m_",c,"_n_1.csv"), row.names = FALSE)
    }
  }
}
