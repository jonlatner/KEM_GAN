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

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"

setwd(main_dir)

#functions
options(scipen=999) 

# Create fake synthetic data ----

ods <- SD2011
ods[ods < 0] <- NA
ods[ods == ""] <- NA
ods <- ods %>%
  mutate_if(is.character, as.factor)
str(ods)

summary(ods$bmi)

write.csv(ods, paste0(original_data,"sd2011.csv"), row.names = FALSE)
summary(ods)

ods <- SD2011
ods[ods == ""] <- NA
ods[ods < 0] <- NA
ods <- select(ods,sex,age,income,edu)
write.csv(ods, paste0(original_data,"sd2011_small.csv"), row.names = FALSE)

ods <- SD2011
ods[ods == ""] <- NA
ods[ods < 0] <- NA
vars <- c("sex", "age", "edu", "marital", "income", "ls", "wkabint")
ods <- SD2011[, vars]
write.csv(ods, paste0(original_data,"sd2011_medium.csv"), row.names = FALSE)

ods <- SD2011
ods[ods == ""] <- NA
ods[ods < 0] <- NA
ods <- select(ods,age,eduspec,sex,alcabuse)
write.csv(ods, paste0(original_data,"sd2011_duration_w_missing.csv"), row.names = FALSE)
# df_synds <- syn(ods, m = 1)

ods <- SD2011
ods[ods == ""] <- NA
ods[ods < 0] <- NA
ods <- select(ods,age,eduspec,sex,alcabuse)
ods <- na.omit(ods)
write.csv(ods, paste0(original_data,"sd2011_duration_wo_missing.csv"), row.names = FALSE)


# Reorder columns based on the number of levels
ods <- SD2011
ods[ods == ""] <- NA
ods[ods < 0] <- NA
# Count the number of levels for each column
num_levels <- sapply(ods, function(x) length(levels(x)))
ods <- ods[, order(-num_levels)]
write.csv(ods, paste0(original_data,"sd2011_ordered.csv"), row.names = FALSE)

# Reorder columns so bmi is earlier
ods <- SD2011
ods[ods < 0] <- NA
ods[ods == ""] <- NA
ods <- select(ods,sex,age,height,weight)
write.csv(ods, paste0(original_data,"sd2011_bmi.csv"), row.names = FALSE)
summary(ods)

# Reorder columns so bmi is earlier
ods <- SD2011
ods[ods < 0] <- NA
ods[ods == ""] <- NA
ods$bmi <- NULL
write.csv(ods, paste0(original_data,"sd2011_bmi_large.csv"), row.names = FALSE)
summary(ods)

