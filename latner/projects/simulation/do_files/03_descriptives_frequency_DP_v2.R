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
library(tidyverse)
library(ggh4x)
library(VGAM)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"

setwd(main_dir)

#functions
options(scipen=999) 

# Load data ----

ods <- read.csv(paste0(original_data,"simulated.csv"))

# Define the 16 possible combinations of four binary variables
combinations <- expand.grid(y1 = c(0, 1), y2 = c(0, 1), y3 = c(0, 1), y4 = c(0, 1))

# Generate laplace noise ---- 

epsilon <- c(.1,.25,.5,1)
df_compare <- data.frame()
for (e in epsilon) {
  for (c in 1:10) {
    for (r in 1:16) {

      # load original data 
      df_ods <- ods
      
      # Drop the last row
      df_ods <- head(ods, -1)
      
      # Set the last observation to last_record
      last_record <- combinations[r,]
      print(last_record)
      df_ods[1000,] <- last_record

      # Create a frequency table for true original data (unique = 1111)
      df_ods_frequency <- df_ods
      df_ods_frequency$combine <- paste(df_ods_frequency$var1, df_ods_frequency$var2, df_ods_frequency$var3, df_ods_frequency$var4, sep = "")
      df_ods_frequency <- df_ods_frequency %>%
        select(-matches("var"))
      
      df_ods_frequency <- as.data.frame(table(df_ods_frequency)) 

      # Check if number of observations is less than 16 and add a row with Freq = 0 if combine = "1111"
      if (nrow(df_ods_frequency) < 16) {
        df_ods_frequency <- df_ods_frequency %>%
          add_row(combine = "1111", Freq = 0)
      }
      
      df_ods_frequency <- df_ods_frequency %>%
        rename(value=Freq) %>%
        mutate(type = "original",
               n = 1,
               dp = e,
               unique = paste(last_record$y1, last_record$y2, last_record$y3, last_record$y4, sep = ""))
      
      
      # Parameters for the Laplace distribution
      location <- 0  # Mean (location parameter)
      sensitivity <- 1 # this is always 1 in integer counting queries (if integers were 2,4,6,etc., then sensitivity would be 2)
      b <- sensitivity/e # scale parameter
      
      # Generate Laplace noise for each value in n
      laplace_noise <- rlaplace(nrow(df_ods_frequency), location, b)
      
      # Add the noise to the n column
      df_sds_frequency <- df_ods_frequency
      df_sds_frequency$value <- round(df_sds_frequency$value + laplace_noise,0)
      df_sds_frequency <- df_sds_frequency %>%
        mutate(type = "synthetic",
               dp = e,
               n = c,
               unique = paste(last_record$y1, last_record$y2, last_record$y3, last_record$y4, sep = ""))
      df_compare <- rbind(df_compare,df_ods_frequency,df_sds_frequency)
    }
  }
}

# Save ----

write.csv(df_compare, paste0(synthetic_data,"frequency_dp.csv"), row.names = FALSE)

# Plot the percent frequency histogram by levels of differential privacy ----

df_compare <- read_csv(paste0(synthetic_data,"frequency_dp.csv"))

df_graph_sds <- df_compare %>%
  filter(type == "synthetic") %>%
  arrange(type,dp,n) %>%
  mutate(dp = as.factor(dp))

df_graph_ods <- df_compare %>%
  filter(type == "original") %>%
  mutate(dp = as.factor(dp))
df_graph_ods <- unique(df_graph_ods)

df_graph <- 
  ggplot() +
  geom_boxplot(position = position_dodge(width=0.9), aes(x = combine, y = value, fill = type), data = df_graph_sds, alpha = .2) +
  geom_bar(data = df_graph_ods, aes(x = combine, y = value, fill = type), position = position_dodge(width=0.9), stat = "identity", alpha = .2) +
  facet_grid(dp~unique, labeller = "label_both") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 25, hjust = 1, vjust = .5),
        axis.title.x = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"frequency_dp.pdf"), height = 8, width = 10)

