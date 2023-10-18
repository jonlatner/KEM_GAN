# https://www.r-bloggers.com/2021/05/how-to-generate-correlated-data-in-r/
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
library(MASS)
library(tidyverse)
library(GGally)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/IAB/simulation_data/categorical_dim//"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"

setwd(main_dir)

# Generate Correlated Data ----

set.seed(1234)

# create the variance covariance matrix
sigma<-rbind(c(1,-0.8,-0.7), c(-0.8,1, 0.9), c(-0.7,0.9,1))

# create the mean vector
mu<-c(10, 5, 2) 

# generate the multivariate normal distribution
df<-as.data.frame(mvrnorm(n=1000, mu=mu, Sigma=sigma))

ggpairs(df)

# Generate Categorical Correlated Data ----

# Binary variables
df<-df%>%mutate(MyBinary = ifelse(V1>median(V1), 1 ,0))

# Binary variables with noise

# Rules:
# 1) If the variable is greater than the median, then assign 1 with a 75% probability
# 2) If the variable is less than the median, then assign 1 with a 25% probability

df<-df%>%mutate(MyNoisyBinary = ifelse(V1>median(V1), sample(c(0,1),n(), replace = TRUE, p=c(0.25, 0.75)) ,
                                       sample(c(0,1),n(), replace = TRUE, p=c(0.75, 0.25))))

# Categorical variables

# Rules:
# 1) If it is less than the Q1 then Age Group 1
# 2) If it is less than the Q2 then Age Group 2
# 3) if it is less than the Q3 then Age Group 3
# 4) else, Age Group 4

df<-df%>%mutate(AgeGroup= case_when(V1<quantile(V1,0.25)~"Group 1",
                                    V1<quantile(V1,0.5)~"Group 2",
                                    V1<quantile(V1,0.75)~"Group 3",
                                    TRUE~"Group 4"))

# Categorical variables with noise

df<-df%>%mutate(MyNoisyCat= case_when(V1<quantile(V1,0.25)~sample(c("G1","G2","G3","G4"),n(), replace = TRUE, p=c(0.70, 0.1, 0.1, 0.1)),
                                      V1<quantile(V1,0.5)~sample(c("G1","G2","G3","G4"),n(), replace = TRUE, p=c(0.1, 0.7, 0.1, 0.1)),
                                      V1<quantile(V1,0.75)~sample(c("G1","G2","G3","G4"),n(), replace = TRUE, p=c(0.1, 0.1, 0.7, 0.1)),
                                      TRUE~sample(c("G1","G2","G3","G4"),n(), replace = TRUE, p=c(0.1, 0.1, 0.1, 0.7))))


ggpairs(df)
