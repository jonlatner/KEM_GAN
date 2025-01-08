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

df_ods <- SD2011[, c("sex","edu","age","income","depress")]

# original ----
s5 <- syn(df_ods, seed = 8564, m = 1, print.flag = FALSE)
t5 <- disclosure( s5, df_ods, keys = c("sex","edu","age","income"), target = "depress", print.flag = FALSE)

# modified ----
s6 <- s5
# s6$syn$depress <- recode(s6$syn$depress, "4:10=2; 11:hi=3")
# s6$syn$depress <- 1
t6 <- disclosure( s6, df_ods, keys = c("sex","edu","age","income"), target = "depress", print.flag = FALSE)

# risk measure ----
print(t5, to.print = c("attrib"))
print(t6, to.print = c("attrib"))
