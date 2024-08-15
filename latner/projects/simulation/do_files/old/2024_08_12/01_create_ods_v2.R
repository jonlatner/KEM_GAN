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

# loop ----
# create 16 datasets with different unique combinations of the last record (1000th)
# the real last record is 1111

df_combine <- data.frame()
for (r in 1:16) {
  
  # Define last_record and all_others
  last_record <- combinations[r,]
  all_others <- combinations[-r,]
  
  # Initialize the dataset
  D <- data.frame(matrix(ncol = 4, nrow = n))
  colnames(D) <- c("var1", "var2", "var3", "var4")
  
  # Sample the first 999 observations from all_others with equal probability
  for (i in 1:(n-1)) {
    sampled_row <- sample(1:15, 1)
    D[i,] <- all_others[sampled_row,]
  }
  
  # Set the 1000th observation to last_record
  D[1000,] <- last_record
  
  # Convert to data frame and print the first few rows
  df_ods <- as.data.frame(D)

  # Save
  write.csv(df_ods, paste0(original_data,"simulated_",r,".csv"), row.names = FALSE)
  
  # Assign
  df_ods$unique <- paste(last_record$y1, last_record$y2, last_record$y3, last_record$y4, sep = "")
  
  df_combine <- rbind(df_combine,df_ods)
}


# Compare frequency ----
head(df_combine)

# Original data
df_combine$combine <- paste(df_combine$var1, df_combine$var2, df_combine$var3, df_combine$var4, sep = "")
df_combine <- df_combine %>%
  select(-matches("var"))
df_frequency <- as.data.frame(table(df_combine))
df_frequency

ggplot(df_frequency, aes(x = combine, y = Freq)) +
  facet_wrap(~unique, labeller = "label_both") + 
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



