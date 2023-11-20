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
library(synthpop)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/IAB/simulation_data/categorical_dim/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
duration = "duration/"

setwd(main_dir)


# Synthesize data grid ----

# Dimensions
rows = c(1000, 5000) # Rows/observations
cols = c(20) # Columns/variables
vals = c(10,15,20)  # Number of possible options for each character

my.seed = 1230

df_duration <- data.frame(
  type = as.character(),
  rows = as.numeric(),
  cols = as.numeric(),
  vals = as.numeric(),
  n = as.numeric(),
  duration = as.numeric()
)

for (r in rows) {
  for (c in cols) {
    for (v in vals) {
      
      print(paste(r, c, v,sep = ","))
      
      df_ods <- read.csv(paste0(original_data,"ods_rows_",r,"_cols_",c,"_vals_",v,".csv"))
      my.seed <- my.seed + 1
      
      time_start <- proc.time()
      
      sds.default <- syn(df_ods,
                         m = 1,
                         seed = my.seed)
      
      time_end <- proc.time()
      
      time_duration <- as.numeric(time_end[1] - time_start[1])
      
      output = data.frame(type="synthpop",
                          rows=r,
                          cols=c,
                          vals=v,
                          n=1,
                          duration=time_duration)
      df_duration <- rbind(df_duration,output)
      
      synthpop_df <- sds.default$syn
      write.csv(synthpop_df, file = paste0(synthetic_data,"sds_synthpop_rows_",r,"_cols_",c,"_vals_",v,"_n_1.csv"), row.names = FALSE)

    }
  }
}

write.csv(df_duration, file = paste0(duration,"duration_synthpop.csv"), row.names = FALSE)

df_duration

