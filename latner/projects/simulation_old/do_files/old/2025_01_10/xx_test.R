library(tidyverse)
library(synthpop)

# Create Ground Truth (GT: df_ods)
df_ods <- data.frame(
  var1 = c(0, 0, 0, 1, 1),
  var2 = c(0, 0, 1, 0, 1),
  var3 = c(0, 1, 0, 0, 1),
  var4 = c(0, 1, 1, 0, 1)  # Target variable (t)
)

# Create a composite key for GT
df_ods$q <- paste(df_ods$var1, df_ods$var2, df_ods$var3, sep = "_")

# Create Synthetic Data (SD: df_sds)
df_sds <- data.frame(
  var1 = c(0, 0, 0, 0, 1),
  var2 = c(0, 0, 0, 1, 0),
  var3 = c(0, 1, 1, 0, 0),
  var4 = c(0, 1, 0, 1, 0)  # Target variable (t)
)

# Create a composite key for SD
df_sds$q <- paste(df_sds$var1, df_sds$var2, df_sds$var3, sep = "_")
df_sds_1 <- df_sds

# Create Synthetic Data (SD: df_sds)
df_sds <- data.frame(
  var1 = c(0, 0, 0, 0, 1),
  var2 = c(0, 0, 0, 1, 0),
  var3 = c(0, 1, 1, 0, 0),
  var4 = c(1, 1, 1, 1, 1)  # Target variable (t)
)

# Create a composite key for SD
df_sds$q <- paste(df_sds$var1, df_sds$var2, df_sds$var3, sep = "_")
df_sds_2 <- df_sds

t1 <- disclosure(df_sds_1, df_ods, keys = c("var1", "var2", "var3"), target = "var4", print.flag = FALSE)
t2 <- disclosure(df_sds_2, df_ods, keys = c("var1", "var2", "var3"), target = "var4", print.flag = FALSE)
print(t1, to.print = c("attrib"))
print(t2, to.print = c("attrib"))

# Step 1: Identify disclosive keys in SD
disclosive_keys <- df_sds_1 %>%
  group_by(q) %>%
  summarize(
    disclosive = n_distinct(var4) == 1,  # Check if var4 (t) is constant
    var4_sd = ifelse(disclosive, unique(var4), NA),  # Get the constant value if disclosive
    .groups = "drop"
  )

# Step 2: Merge disclosive information into GT (df_ods)
df_merged_1 <- df_ods %>%
  left_join(disclosive_keys, by = "q") %>%
  mutate(
    disclosive = ifelse(is.na(disclosive), FALSE, disclosive),  # Mark non-matching q as not disclosive
    correct = disclosive & (var4 == var4_sd)  # Check if t matches
  )

# Step 3: Calculate DiSCO
DiSCO_1 <- 100 * mean(df_merged_1$correct, na.rm = TRUE)

# Output DiSCO
DiSCO_1


# Step 1: Identify disclosive keys in SD
disclosive_keys <- df_sds_2 %>%
  group_by(q) %>%
  summarize(
    disclosive = n_distinct(var4) == 1,  # Check if var4 (t) is constant
    var4_sd = ifelse(disclosive, unique(var4), NA),  # Get the constant value if disclosive
    .groups = "drop"
  )

# Step 2: Merge disclosive information into GT (df_ods)
df_merged_2 <- df_ods %>%
  left_join(disclosive_keys, by = "q") %>%
  mutate(
    disclosive = ifelse(is.na(disclosive), FALSE, disclosive),  # Mark non-matching q as not disclosive
    correct = disclosive & (var4 == var4_sd)  # Check if t matches
  )

# Step 3: Calculate DiSCO
DiSCO_1 <- 100 * mean(df_merged_2$correct, na.rm = TRUE)

# Output DiSCO
DiSCO_1
