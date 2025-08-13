library(tidyverse)
library(plm)
library(texreg)

options(scipen=999) 

df <- read.csv(paste0("/Users/jonathanlatner/Desktop/minijob.csv"),sep = ";")
df

model <- lm(dv~mini + as.factor(firm), data = df)
screenreg(model)
