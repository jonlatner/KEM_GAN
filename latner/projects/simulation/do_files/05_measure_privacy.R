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
original_data = "data_files/original/"
tables = "tables/"

setwd(main_dir)

#functions
options(scipen=999) 

# Set seed for reproducibility
my.seed = 1237
# my.seed = 1240 # reproduces 3 observation

set.seed(my.seed)

# Load original data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))

# Create synthetic data with numeric variables ----


# Identity disclosure measures ----

sds <- syn(df_ods, m = 100, seed = my.seed)
t1 <- disclosure.summary(sds, df_ods, print.flag = FALSE, plot = TRUE, keys = c("var1", "var2", "var3"), target = "var4")
print(t1, plot = FALSE)

print(t1, plot = FALSE, to.print = "ident")
print(t1, plot = FALSE, to.print = "attrib")
test <- replicated.uniques(sds, df_ods)

# Create 5 synthetic data sets ----

sds <- syn(df_ods, m = 10, seed = my.seed)
df_sds <- data.frame(sds$syn)
df_sds$combine <- paste(df_sds$var1, df_sds$var2, df_sds$var3, df_sds$var4, sep = "")
df_sds <- df_sds %>%
  select(-matches("var"))
df_sds_frequency <- as.data.frame(table(df_sds))
df_sds_frequency

my.seed = 1237
# my.seed = 1240 # reproduces 3 observation
sds <- syn(df_ods, m = 5, seed = my.seed)
df_sds <- data.frame(sds$syn[4])
df_sds$combine <- paste(df_sds$var1, df_sds$var2, df_sds$var3, df_sds$var4, sep = "")
df_sds <- df_sds %>%
  select(-matches("var"))
df_sds_frequency <- as.data.frame(table(df_sds))
df_sds_frequency
t1 <- disclosure.summary(sds, df_ods, print.flag = FALSE, plot = TRUE, keys = c("var1", "var2", "var3"), target = "var4")
print(t1, plot = FALSE)
print(t1, plot = FALSE, to.print = "attrib")
t1$output.list$var4$attrib

replicated.uniques(sds, df_ods)

