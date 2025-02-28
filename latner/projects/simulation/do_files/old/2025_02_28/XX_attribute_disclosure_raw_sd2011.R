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

#functions
options(scipen=999) 

# Load data ----

df_ods <- SD2011[, c("sex", "age", "region","placesize","depress")]
# df_ods <- na.omit(df_ods)
df_ods[is.na(df_ods)] <- -8

# original ----
s5 <- syn(df_ods, seed = 8564, m = 1, print.flag = FALSE)
t5 <- disclosure( s5, df_ods, keys = c("sex", "age", "region", "placesize"), target = "depress", print.flag = FALSE)
df_sds <- s5$syn

t5
print(t5, to.print = c("ident"))
print(t5, to.print = c("attrib"))


# Identity disclosure measures ----

# Calculate DiSCO by hand
# Step 1: Create composite key q for GT and SD
df_ods$q <- paste(df_ods$sex, df_ods$age, df_ods$region, df_ods$placesize, sep = "_")
df_sds$q <- paste(df_sds$sex, df_sds$age, df_sds$region, df_sds$placesize, sep = "_")

# Step 2: Calculate iS
# iS : Proportion of all records in GT whose  q  value is found in SD.

iS <- 100 * mean(df_ods$q %in% df_sds$q)
iS

# Step 3: Calculate DiS
# DiS : Proportion of all records in GT where  q  in SD is disclosive (i.e.,  t  values for  q  are constant in SD).

DiS <- 100 * mean(sapply(1:nrow(df_ods), function(i) {
  q <- df_ods$q[i]
  sd_subset <- df_sds[df_sds$q == q, ]
  length(unique(sd_subset$depress)) == 1  # Check if t values are constant
}))
DiS 


# Step 4: Calculate DiSCO (Proportion of records in GT where q is disclosive and matches t in SD)
# DiSCO : Proportion of all records in GT where  q  in SD is disclosive and the disclosed  t  value matches the true  t  value in GT.

DiSCO <- 100 * mean(sapply(1:nrow(df_ods), function(i) {
  q <- df_ods$q[i]
  sd_subset <- df_sds[df_sds$q == q, ]
  if (length(unique(sd_subset$depress)) == 1) {  # Check if q is disclosive
    gt_value <- df_ods$depress[i]
    sd_value <- unique(sd_subset$depress)
    return(gt_value == sd_value)  # Check if t matches
  } else {
    return(FALSE)
  }
}))

DiSCO 

# Output the results
cat("iS:", iS, "%\n")
cat("DiS:", DiS, "%\n")
cat("DiSCO:", DiSCO, "%\n")
print(t5, to.print = c("attrib"))

