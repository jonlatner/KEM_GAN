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

set.seed(my.seed)

# Load original data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))

# Disclosure measures ----

sds <- syn(df_ods, m = 1, seed = my.seed)

# create summary table
t1 <- disclosure.summary(sds, df_ods, print.flag = FALSE, plot = TRUE, keys = c("var1", "var2", "var3"), target = "var4")

ident = print(t1, plot = FALSE, to.print = "ident")
attrib = print(t1, plot = FALSE, to.print = "attrib")

df_risk <- data.frame(
  data = c("Original", "Synthetic"),
  identity = c(t1$ident.orig,t1$ident.syn),
  attribute = c(t1$attrib.table$attrib.orig, t1$attrib.table$attrib.syn)
)

df_risk

# Create the xtable object
latex_table <- xtable(df_risk)

print.xtable(latex_table, 
             include.rownames = FALSE, 
             # include.colnames = FALSE, 
             floating = FALSE,
             booktabs = TRUE, 
             file = paste0(tables,"table_disclosure_risk_1.tex"))

# Create 10 synthetic data sets ----

df_sds <- syn(df_ods, m = 10)

for (c in 1:10) {
  print(c)
  
  # Create fake synthetic data
  sds <- syn(df_ods, m = 1, seed = my.seed)
  df_sds$syn[[c]] <- sds$syn

  # create seed
  my.seed = my.seed + 1
}

# create summary table
t1 <- disclosure.summary(df_sds, df_ods, print.flag = FALSE, plot = TRUE, keys = c("var1", "var2", "var3"), target = "var4")

df_risk <- data.frame(
  data = c("Original", "Synthetic"),
  identity = c(t1$ident.orig,t1$ident.syn),
  attribute = c(t1$attrib.table$attrib.orig, t1$attrib.table$attrib.syn)
)

df_risk

# Create the xtable object
latex_table <- xtable(df_risk)

print.xtable(latex_table, 
             include.rownames = FALSE, 
             # include.colnames = FALSE, 
             floating = FALSE,
             booktabs = TRUE, 
             file = paste0(tables,"table_disclosure_risk_10_summary.tex"))

# create detailed table

t1 <- disclosure(df_sds, df_ods, print.flag = FALSE, plot = TRUE, keys = c("var1", "var2", "var3"), target = "var4")


repU <- t1$ident$repU
average_row <- mean(repU) # calculate average row across 10 synthetic data sets
repU <- c(0, repU, average_row)

DiSCO <- t1$attrib$DiSCO
average_row <- mean(DiSCO) # calculate average row across 10 synthetic data sets
DiSCO <- c(0, DiSCO, average_row)

# create table
df_risk <- data.frame(
  data = c("Original", "Synthetic 1", "Synthetic 2", "Synthetic 3", "Synthetic 4", "Synthetic 5", "Synthetic 6", "Synthetic 7", "Synthetic 8", "Synthetic 9", "Synthetic 10", "Average"),
  identity = c(repU),
  attribute = c(DiSCO)
)


# Create the xtable object
latex_table <- xtable(df_risk,align = "llrr")

print.xtable(latex_table, 
             include.rownames = FALSE, 
             # include.colnames = FALSE, 
             floating = FALSE,
             booktabs = TRUE, 
             sanitize.text.function = function(x) {x},
             file = paste0(tables,"table_disclosure_risk_10.tex"))
