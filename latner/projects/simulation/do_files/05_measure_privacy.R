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

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
tables = "tables/"

setwd(main_dir)

#functions
options(scipen=999) 

# Set seed for reproducibility
my.seed = 1234
set.seed(my.seed)

# Load original data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))

# Create synthetic data with numeric variables ----

sds <- syn(df_ods, m = 1, seed = my.seed)

# Identity disclosure measures ----

replicated.uniques (sds, df_ods)
                    
# table ----

t <- xtable(df_table, digits = 4)

align(t) <- c("l", "l","l", "l") 

hline_top <- c("\\toprule \n")
hline_bot <- c("\\bottomrule \n")

# Define a custom function to sanitize the column names
sanitize_colnames <- function(x) {
  gsub("_", "\\\\_", x)
}


print(t, 
      file = paste0(tables,"table_utility_measure.tex"),
      include.rownames = TRUE, 
      include.colnames = TRUE,
      sanitize.text.function = sanitize_colnames,
      floating="FALSE",
      hline.after = NULL,
      add.to.row = list(
        pos = list(-1,0,4,5),
        command = c("\\toprule \n",
                    "\\midrule \n",
                    "\\midrule \n",
                    "\\bottomrule \n")),
      comment = FALSE
)

t
