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
synthetic_data = "data_files/synthetic/synthpop/"
graphs = "graphs/"

setwd(main_dir)

#functions
options(scipen=999) 


# Set seed for reproducibility
my.seed = 1237
set.seed(my.seed)

# Load data ----

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
synthetic_data = "data_files/synthetic/synthpop/"
tables = "tables/"

setwd(main_dir)

#functions
options(scipen=999) 



# Load data ----

ods <- SD2011[, c("sex", "age", "region","placesize","depress")]

# original ----
s5 <- syn(ods, seed = 8564, m = 5, print.flag = FALSE)
t5 <- disclosure( s5, ods, keys = c("sex", "age", "region", "placesize"), target = "depress", print.flag = FALSE)
print(t5, to.print = c("ident"))
print(t5, to.print = c("attrib"))

df_attribute <- data.frame(t5$attrib) %>%
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
             file = paste0(tables,"table_attribute_risk_sd2011_original.tex"))


# modified ----

s6 <- s5
for (c in 1:5) {
  sds <- data.frame(s5$syn[c])
  sds$depress <- 1
  s6$syn[[c]] <- sds
}

t6 <- disclosure( s6, ods, keys = c("sex", "age", "region", "placesize"), target = "depress", print.flag = FALSE)


df_attribute <- data.frame(t6$attrib) %>%
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
             file = paste0(tables,"table_attribute_risk_sd2011_modified.tex"))

print(t5, to.print = c("attrib"))
print(t6, to.print = c("attrib"))
