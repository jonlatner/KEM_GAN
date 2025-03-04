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

# Create synthetic data with numeric variables ----

# my.seed = 1237 # reproduces 1 observation
my.seed = 1238 # reproduces 0 unique observations
# my.seed = 1235 # reproduces 0 unique observations
# my.seed = 1240 # reproduces 3 unique observations

sds <- syn(df_ods, m = 1, seed = my.seed)
t1 <- disclosure.summary(sds, df_ods, print.flag = FALSE, plot = TRUE, keys = c("var1", "var2", "var3"), target = "var4")
print(t1, plot = FALSE)
t1$output.list$var4$attrib

df_sds <- sds$syn
df_sds$combine <- paste(df_sds$var1, df_sds$var2, df_sds$var3, df_sds$var4, sep = "")
df_sds <- df_sds %>%
  select(-matches("var"))
df_sds_frequency <- as.data.frame(table(df_sds))
df_sds_frequency

df_sds <- sds$syn

# Identity disclosure measures ----

# Calculate DiSCO by hand
# Step 1: Create composite key q for GT and SD
df_ods$q <- paste(df_ods$var1, df_ods$var2, df_ods$var3, sep = "_")
df_sds$q <- paste(df_sds$var1, df_sds$var2, df_sds$var3, sep = "_")

# Step 2: Calculate iS
# iS : Proportion of all records in GT whose  q  value is found in SD.

iS <- 100 * mean(df_ods$q %in% df_sds$q)
iS

# Step 3: Calculate DiS
# DiS : Proportion of all records in GT where  q  in SD is disclosive (i.e.,  t  values for  q  are constant in SD).

DiS <- 100 * mean(sapply(1:nrow(df_ods), function(i) {
  q <- df_ods$q[i]
  sd_subset <- df_sds[df_sds$q == q, ]
  length(unique(sd_subset$var4)) == 1  # Check if t values are constant
}))
DiS 

# Assuming df_ods and df_sds are the ground truth and synthetic data frames
# Step 1: Identify disclosive keys in SD
disclosive_keys <- df_sds %>%
  group_by(q) %>%                # Group by composite key q
  summarize(disclosive = n_distinct(var4) == 1, .groups = "drop") # Check if t is constant

# Step 2: Merge disclosive status back to GT (df_ods)
df_ods_dis <- df_ods %>%
  left_join(disclosive_keys, by = "q") %>%
  mutate(disclosive = ifelse(is.na(disclosive), FALSE, disclosive)) # Handle missing keys

# Step 3: Calculate DiS
DiS <- 100 * mean(df_ods_dis$disclosive)
DiS

df_ods_dis_true <- df_ods_dis %>%
  filter(disclosive == TRUE)

t1$output.list$var4$attrib

# Step 4: Calculate DiSCO (Proportion of records in GT where q is disclosive and matches t in SD)
# DiSCO : Proportion of all records in GT where  q  in SD is disclosive and the disclosed  t  value matches the true  t  value in GT.

DiSCO <- 100 * mean(sapply(1:nrow(df_ods), function(i) {
  q <- df_ods$q[i]
  sd_subset <- df_sds[df_sds$q == q, ]
  if (length(unique(sd_subset$var4)) == 1) {  # Check if q is disclosive
    gt_value <- df_ods$var4[i]
    sd_value <- unique(sd_subset$var4)
    return(gt_value == sd_value)  # Check if t matches
  } else {
    return(FALSE)
  }
}))

DiSCO # in this case this is the equivalent 66/1000 ((65/1000 = var1=1,var2=2,var3=1)+(1/1000 = var1=1,var2=2,var3=1,var4=1))

# Step 1: Identify disclosive keys in SD
disclosive_keys <- df_sds %>%
  group_by(q) %>%
  summarize(
    disclosive = n_distinct(var4) == 1,  # Check if var4 (t) is constant
    var4_sd = ifelse(disclosive, unique(var4), NA),  # Get the constant value if disclosive
    .groups = "drop"
  )

# Step 2: Merge disclosive information into GT (df_ods)
df_merged <- df_ods %>%
  left_join(disclosive_keys, by = "q") %>%
  mutate(
    disclosive = ifelse(is.na(disclosive), FALSE, disclosive),  # Mark non-matching q as not disclosive
    correct = disclosive & (var4 == var4_sd)  # Check if t matches
  )

# Step 3: Calculate DiSCO
DiSCO <- 100 * mean(df_merged$correct, na.rm = TRUE)

# Output DiSCO
DiSCO


# Output the results
cat("iS:", iS, "%\n")
cat("DiS:", DiS, "%\n")
cat("DiSCO:", DiSCO, "%\n")

t1 <- disclosure.summary(sds, df_ods, print.flag = FALSE, plot = TRUE, keys = c("var1", "var2", "var3"), target = "var4")
print(t1, plot = FALSE)
t1$output.list$var4$attrib
