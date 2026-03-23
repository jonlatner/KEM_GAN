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
library(readr)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
tables = "tables/"
synthetic_data = "data_files/synthetic/synthpop/"

setwd(main_dir)

#functions
options(scipen=999) 

# Set seed for reproducibility
my.seed = 1237

set.seed(my.seed)

# Create simulated data ----


# Number of observations
n <- 1000

# Define the 16 possible combinations of four binary variables
combinations <- expand.grid(y1 = c(0, 1), y2 = c(0, 1), y3 = c(0, 1), y4 = c(0, 1))

# Define c_16 and C_−16
c_16 <- combinations[16,]
C_minus_16 <- combinations[-16,]

# Initialize the dataset
D <- data.frame(matrix(ncol = 4, nrow = n))
colnames(D) <- c("var1", "var2", "var3", "var4")

# Sample the first 999 observations from C_minus_16 with equal probability
for (i in 1:(n-1)) {
  sampled_row <- sample(1:15, 1)
  D[i,] <- C_minus_16[sampled_row,]
}

# Set the 1000th observation to c_16
D[1000,] <- c_16

# Convert to data frame and print the first few rows
df_ods <- as.data.frame(D)
head(df_ods)


# Generate synthetic data ----

df_sds_10 <- data.frame()
for (c in 1:10) {
  
  # Create fake synthetic data
  sds <- syn(df_ods, m = 1, seed = my.seed)
  sds <- sds$syn
  
  # create seed
  my.seed = my.seed + 1
  
  # Create a frequency table for synthetic data
  
  sds$combine <- paste(sds$var1, sds$var2, sds$var3, sds$var4, sep = "")
  sds <- sds %>%
    select(-matches("var"))
  df_sds_frequency <- as.data.frame(table(sds))
  df_sds_frequency$type <- "synthetic"
  df_sds_frequency$n <- c
  
  # Combine
  df_sds_10 <- rbind(df_sds_10,df_sds_frequency)
}

df_sds_10


sds <- syn(df_ods, m = 1, seed = 1237)
df_sds_1 <- sds$syn

# Generate frequency lists ----

# df_ods
df_frequency <- df_ods
df_frequency$combine <- paste(df_frequency$var1, df_frequency$var2, df_frequency$var3, df_frequency$var4, sep = "")
df_frequency <- df_frequency %>%
  select(-matches("var"))
df_frequency_ods <- as.data.frame(table(df_frequency)) 
df_frequency_ods_unique <- df_frequency_ods %>%
  filter(combine == "1111")
df_frequency_ods_unique <- df_frequency_ods_unique$Freq
df_frequency_ods_unique


# df_sds_1
df_frequency_sds_1 <- df_sds_1
df_frequency_sds_1$combine <- paste(df_frequency_sds_1$var1, df_frequency_sds_1$var2, df_frequency_sds_1$var3, df_frequency_sds_1$var4, sep = "")
df_frequency_sds_1 <- df_frequency_sds_1 %>%
  select(-matches("var"))
df_frequency_sds_1 <- as.data.frame(table(df_frequency_sds_1)) %>%
  filter(combine == "1111")
sds_1_unique <- df_frequency_sds_1$Freq
sds_1_unique

# df_sds_10

df_frequency_ods$n <- 0
df_frequency_ods$type <- "original"
df_frequency_ods$n <- 0
df_frequency_sds_10 <- rbind(df_frequency_ods,df_sds_10) %>%
  pivot_wider(names_from = "n", values_from = "Freq")  %>%
  filter(combine == "1111") %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  pivot_longer(
    cols = `0`:`10`,        # the range of columns to pivot
    names_to = "value",     # new column that stores the old column names
    values_to = "count",     # new column that stores the numbers
    ) %>%
  filter((type == "original" & value == 0) | (type == "synthetic" & value > 0))
df_frequency_sds_10 %>% print(n=22)
df_frequency_sds_10_unique <- as.vector(df_frequency_sds_10$count)
df_frequency_sds_10_unique

# Disclosure measures ----


# create summary table
t1 <- multi.disclosure(sds, df_ods, print.flag = FALSE, plot = TRUE, keys = c("var1", "var2", "var3"), target = "var4")

ident = print(t1, plot = FALSE, to.print = "ident")
attrib = print(t1, plot = FALSE, to.print = "attrib")

ttest <- print(t1, plot = FALSE, to.print = "allCAPs")
ttest <- print(t1, plot = FALSE, to.print = "TCAP")
ttest

df_risk <- data.frame(
  data = c("Original", "Synthetic"),
  unique = c(df_frequency_ods_unique,sds_1_unique),
  identity = c(t1$ident.orig,t1$ident.syn),
  attribute = c(t1$attrib.table$attrib.orig, t1$attrib.table$attrib.syn)
)

df_risk

# Create the xtable object
latex_table <- xtable(df_risk)

colnames(latex_table) <- c("Data", "Unique", "Identity Risk ($repU$)", "Attribute Risk ($DiSCO$)")

print.xtable(latex_table, 
             include.rownames = FALSE, 
             floating = FALSE,
             booktabs = TRUE, 
             sanitize.text.function = identity,
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
t1 <- multi.disclosure(df_sds, df_ods, print.flag = FALSE, plot = TRUE, keys = c("var1", "var2", "var3"), target = "var4")

t10 <- disclosure(df_sds, df_ods, keys = c("var1", "var2", "var3"), target = "var4", print.flag = FALSE)

ttest10 <- print(t10, to.print = "allCAPs")

df_risk <- data.frame(
  data = c("Original", "Synthetic"),
  identity = c(t1$ident.orig,t1$ident.syn),
  attribute = c(t1$attrib.table$attrib.orig, t1$attrib.table$attrib.syn)
)

df_risk

# Table ----
t1 <- disclosure(df_sds, df_ods, print.flag = FALSE, plot = TRUE, keys = c("var1", "var2", "var3"), target = "var4")

repU <- t1$ident$repU
average_row <- mean(repU) # calculate average row across 10 synthetic data sets
repU <- c(0, repU, average_row)

DiSCO <- t1$attrib$DiSCO
average_row <- mean(DiSCO) # calculate average row across 10 synthetic data sets
DiSCO <- c(0, DiSCO, average_row)

df_frequency_sds_10_unique <- c(df_frequency_sds_10_unique, NA)

# create table
df_risk <- data.frame(
  data = c("Original", "Synthetic 1", "Synthetic 2", "Synthetic 3", "Synthetic 4", "Synthetic 5", "Synthetic 6", "Synthetic 7", "Synthetic 8", "Synthetic 9", "Synthetic 10", "Average"),
  unique = df_frequency_sds_10_unique,
  identity = c(repU),
  attribute = c(DiSCO)
)

df_risk

# Create the xtable object
latex_table <- xtable(df_risk,align = "llrrr")

colnames(latex_table) <- c("Data", "Unique", "Identity Risk ($repU$)", "Attribute Risk ($DiSCO$)")

print.xtable(latex_table, 
             include.rownames = FALSE, 
             # include.colnames = FALSE, 
             floating = FALSE,
             booktabs = TRUE, 
             sanitize.text.function = function(x) {x},
             file = paste0(tables,"table_disclosure_risk_10.tex"),
             add.to.row = list(pos = list(nrow(latex_table) - 1),
                               command = "\\midrule \n"))

