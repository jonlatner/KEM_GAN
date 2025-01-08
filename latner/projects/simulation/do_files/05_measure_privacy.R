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
t1 <- disclosure.summary(sds, df_ods, print.flag = FALSE, plot = TRUE, keys = c("var1", "var2", "var3"), target = "var4")
ident = print(t1, plot = FALSE, to.print = "ident")
attrib = print(t1, plot = FALSE, to.print = "attrib")
unique <- data.frame(replicated.uniques(sds, df_ods)$res_tab)
unique

# create table
df_risk <- data.frame(
  data = c("Original", "Synthetic"),
  identity = c(t1$ident.orig,t1$ident.syn),
  unique = c(unique$Number[1], unique$Number[2]),
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

t1 <- disclosure.summary(df_sds, df_ods, print.flag = FALSE, plot = TRUE, keys = c("var1", "var2", "var3"), target = "var4")
ident = print(t1, plot = FALSE, to.print = "ident")
attrib = print(t1, plot = FALSE, to.print = "attrib")
unique <- data.frame(replicated.uniques(df_sds, df_ods)$res_tab)

# create table
df_risk <- data.frame(
  data = c("Original", "Synthetic"),
  identity = c(t1$ident.orig,t1$ident.syn),
  unique = c(unique$Number[1], "see table \\ref{table:frequency_10_data_sets}"),
  attribute = c(t1$attrib.table$attrib.orig, t1$attrib.table$attrib.syn)
)


# Create the xtable object
latex_table <- xtable(df_risk,align = "llrrr")

print.xtable(latex_table, 
             include.rownames = FALSE, 
             # include.colnames = FALSE, 
             floating = FALSE,
             booktabs = TRUE, 
             sanitize.text.function = function(x) {x},
             file = paste0(tables,"table_disclosure_risk_10.tex"))

# Attribute risk measures ----

t1 <- disclosure.summary(df_sds, df_ods, print.flag = FALSE, plot = TRUE, keys = c("var1", "var2", "var3"), target = "var4")
df_attribute <- data.frame(t1$output.list$var4$attrib) %>%
  mutate(m = as.character(row_number())) %>%
  select(m, Dsyn, iS, DiS, DiSCO) 
df_attribute

# Calculate averages for numeric columns
average_row <- colMeans(df_attribute[, sapply(df_attribute, is.numeric)])

# Add a label for the average row
average_row <- c(m = "Average", average_row)

# Bind the average row to the original dataframe
df_attribute <- rbind(df_attribute, average_row)
df_attribute

latex_table <- xtable(df_attribute,align = "llrrrr")

print.xtable(latex_table, 
             include.rownames = FALSE, 
             # include.colnames = FALSE, 
             floating = FALSE,
             booktabs = TRUE, 
             file = paste0(tables,"table_attribute_risk_10.tex"))
