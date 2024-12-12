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

# https://arxiv.org/pdf/2406.16826

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
df_ods = "data_files/original/"
tables = "tables/"

setwd(main_dir)

#functions
options(scipen=999) 

# Set seed for reproducibility
# my.seed = 1237 # reproduces 1 observation
# my.seed = 1238 # reproduces 0 unique observations
my.seed = 1235 # reproduces 0 unique observations

# my.seed = 1240 # reproduces 3 unique observations

set.seed(my.seed)

# Load original data ----

df_ods <- read.csv(paste0(df_ods,"simulated.csv"))
df_ods <- df_ods

# Create synthetic data with numeric variables ----

sds <- syn(df_ods, m = 1, seed = my.seed)
df_sds <- sds$syn
df_sds$combine <- paste(df_sds$var1, df_sds$var2, df_sds$var3, df_sds$var4, sep = "")
df_sds <- df_sds %>%
  select(-matches("var"))
df_sds_frequency <- as.data.frame(table(df_sds))
df_sds_frequency

df_sds <- sds$syn

# Identity disclosure measures ----

t1 <- disclosure.summary(sds, df_ods, print.flag = FALSE, plot = TRUE, keys = c("var1", "var2", "var3"), target = "var4")
print(t1, plot = FALSE)
t1$output.list$var4$attrib

# Calculate DiSCO by hand
# Step 1: Create composite key q for GT and SD
df_ods$q <- paste(df_ods$var1, df_ods$var2, df_ods$var3, sep = "_")
df_sds$q <- paste(df_sds$var1, df_sds$var2, df_sds$var3, sep = "_")

# Step 2: Calculate iS
# iS : Proportion of all records in GT whose  q  value is found in SD.

iS <- 100 * mean(df_ods$q %in% df_sds$q)

# Step 3: Calculate DiS
# DiS : Proportion of all records in GT where  q  in SD is disclosive (i.e.,  t  values for  q  are constant in SD).

DiS <- 100 * mean(sapply(1:nrow(df_ods), function(i) {
  q <- df_ods$q[i]
  sd_subset <- df_sds[df_sds$q == q, ]
  length(unique(sd_subset$var4)) == 1  # Check if t values are constant
}))
DiS # in this case this is the equivalent of 1/15

# Step 4: Calculate DiSCO (Proportion of records in GT where q is disclosive and matches t in SD)
#  DiSCO : Proportion of all records in GT where  q  in SD is disclosive and the disclosed  t  value matches the true  t  value in GT.

DiSCO <- 100 * mean(sapply(1:nrow(df_ods), function(i) {
  q <- df_ods$q[i]
  sd_subset <- df_sds[df_sds$q == q, ]
  if (length(unique(sd_subset$var4)) == 1) {  # Check if q is disclosive
    gt_value <- df_ods$var4[i]
    sd_value <- unique(sd_subset$var4)
    return(gt_value == sd_value)  # Check if t matches
  } else {
    return(FALSE)
  }
}))

DiSCO

# Output the results
cat("iS:", iS, "%\n")
cat("DiS:", DiS, "%\n")
cat("DiSCO:", DiSCO, "%\n")

print(t1, plot = FALSE)
t1$output.list$var4$attrib
