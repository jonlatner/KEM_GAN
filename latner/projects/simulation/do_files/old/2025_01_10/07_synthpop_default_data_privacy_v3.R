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
library(car)

#functions
options(scipen=999) 

# Load data ----

df_sd2011 <- SD2011
df_ods <- df_sd2011[, c("sex", "age", "edu", "income", "depress")]
df_ods <- df_ods %>%
  filter(income>0)
df_ods <- na.omit(df_ods)

# original ----
s5 <- syn(df_ods, seed = 8564, m = 5, print.flag = FALSE)
t5 <- disclosure( s5, df_ods, keys = c("sex", "age", "edu", "income"), target = "depress", print.flag = FALSE)

# modified ----
s6 <- s5
s6$syn[[1]]$depress <- 1
s6$syn[[2]]$depress <- 1
s6$syn[[3]]$depress <- 1
s6$syn[[4]]$depress <- 1
s6$syn[[5]]$depress <- 1
t6 <- disclosure( s6, df_ods, keys = c("sex", "age", "edu", "income"), target = "depress", print.flag = FALSE)

# risk measure ----
print(t5)
print(t6)

