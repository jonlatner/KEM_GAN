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
library(ggh4x) # facet_nested

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"

setwd(main_dir)

#functions
options(scipen=999) 

# Set seed for reproducibility
my.seed = 1234
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
head(df_ods)

# Save ----
write.csv(df_ods, paste0(original_data,"simulated.csv"), row.names = FALSE)

# Create synthetic data ----
sds <- syn(df_ods, m=1, seed = my.seed,minnumlevels = 5,method = "cart")
df_sds_cart <- sds$syn

sds <- syn(df_ods, m=1, seed = my.seed,minnumlevels = 5,method = "ctree")
df_sds_ctree <- sds$syn

# Compare frequency ----

# Original data
df_ods$combine <- paste(df_ods$var1, df_ods$var2, df_ods$var3, df_ods$var4, sep = "")
df_ods <- df_ods %>%
  select(-matches("var"))
ods_frequency <- as.data.frame(table(df_ods))
ods_frequency$type <- "original"

# Synthetic CART
df_sds_cart$combine <- paste(df_sds_cart$var1, df_sds_cart$var2, df_sds_cart$var3, df_sds_cart$var4, sep = "")
df_sds_cart <- df_sds_cart %>%
  select(-matches("var"))
sds_frequency_cart <- as.data.frame(table(df_sds_cart))
sds_frequency_cart$type <- "synthetic (cart)"


# Synthetic CTREE
df_sds_ctree$combine <- paste(df_sds_ctree$var1, df_sds_ctree$var2, df_sds_ctree$var3, df_sds_ctree$var4, sep = "")
df_sds_ctree <- df_sds_ctree %>%
  select(-matches("var"))
sds_frequency_ctree <- as.data.frame(table(df_sds_ctree))
sds_frequency_ctree$type <- "synthetic (ctree)"

# Combine
df_frequency <- rbind(ods_frequency,sds_frequency_cart,sds_frequency_ctree)

ggplot(df_frequency, aes(x = combine, y = Freq, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )



