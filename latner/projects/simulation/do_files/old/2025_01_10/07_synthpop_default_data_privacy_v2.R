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

df_ods <- SD2011[, c("sex","age","region","placesize","depress")]

s5 <- syn(df_ods, seed = 8564, m = 5, print.flag = FALSE)
t5 <- disclosure.summary( s5, df_ods, keys = c("sex","age","region","placesize"), target = "depress", print.flag = FALSE)
t5

replicated.uniques( s5, df_ods)

print(t5, to.print = c("ident"), plot = FALSE)

print(t5, to.print = c("attrib"), plot = FALSE)


# modified ----
s6 <- s5
s6$syn$depress <- 0
s6$syn[[1]]$depress <- 0
s6$syn[[2]]$depress <- 0
s6$syn[[3]]$depress <- 0
s6$syn[[4]]$depress <- 0
s6$syn[[5]]$depress <- 0
t6 <- disclosure( s6, df_ods, keys = c("sex","age","region","placesize"), target = "depress", print.flag = FALSE)
df_syn <- s6$syn

# risk measure ----
print(t5, plot = FALSE)
print(t6, plot = FALSE)

print(t5, to.print = c("attrib"))

print(t6, to.print = c("attrib"))
print(t6, to.print = c("check_1way"))
