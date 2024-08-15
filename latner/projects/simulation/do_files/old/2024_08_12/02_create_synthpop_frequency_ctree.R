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
library(readr)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
graphs = "graphs/"

setwd(main_dir)

#functions
options(scipen=999) 


# Set seed for reproducibility
my.seed = 1234
set.seed(my.seed)

# Load data ----


# Define the 16 possible combinations of four binary variables
combinations <- expand.grid(y1 = c(0, 1), y2 = c(0, 1), y3 = c(0, 1), y4 = c(0, 1))

# Loop ----

df_frequency <- data.frame()
for (c in 1:100) {
  for (r in 1:16) {

    # create seed
    my.seed = my.seed + 1
    
    # Load original data 
    df_ods <- read.csv(paste0(original_data,"simulated.csv"))
    
    # Drop the last row
    df_ods <- head(df_ods, -1)
    
    # Set the last observation to last_record
    last_record <- combinations[r,]
    print(last_record)
    df_ods[1000,] <- last_record

    # Create fake synthetic data
    sds <- syn(df_ods, m = 1, seed = my.seed, method = "ctree")
    sds <- sds$syn
    
    # Create a frequency table for true original data (unique = 1111)
    
    df_ods_frequency <- df_ods
    df_ods_frequency$combine <- paste(df_ods_frequency$var1, df_ods_frequency$var2, df_ods_frequency$var3, df_ods_frequency$var4, sep = "")
    df_ods_frequency <- df_ods_frequency %>%
      select(-matches("var"))
    
    df_ods_frequency <- as.data.frame(table(df_ods_frequency)) %>%
      mutate(type = "original",
             n = c,
             unique = paste(last_record$y1, last_record$y2, last_record$y3, last_record$y4, sep = ""))

    # Create a frequency table for synthetic data
    sds$combine <- paste(sds$var1, sds$var2, sds$var3, sds$var4, sep = "")
    sds <- sds %>%
      select(-matches("var"))
    df_sds_frequency <- as.data.frame(table(sds))
    df_sds_frequency$type <- "synthetic"
    df_sds_frequency$n <- c
    df_sds_frequency$unique <- paste(last_record$y1, last_record$y2, last_record$y3, last_record$y4, sep = "")
    
    # Combine
    df_frequency <- rbind(df_frequency,df_sds_frequency,df_ods_frequency)
  }
}

# Save data ----

write.csv(df_frequency, paste0(synthetic_data,"synthetic_frequency_ctree.csv"), row.names = FALSE, )

# Compare frequency ----

df_frequency <- read_csv(paste0(synthetic_data,"synthetic_frequency_ctree.csv"))

df_graph_sds <- df_frequency %>%
  filter(type == "synthetic") 

df_graph_ods <- df_frequency %>%
  filter(type == "original") 

df_graph_ods <- unique(df_graph_ods)

df_graph <- 
  ggplot() +
  geom_bar(data = df_graph_ods, aes(x = combine, y = Freq, fill = type), position = position_dodge(width=0.9), stat = "identity", alpha = .05) +
  geom_boxplot(position = position_dodge(width=0.9), aes(x = combine, y = Freq, fill = type), data = df_graph_sds, alpha = .2) +
  facet_wrap(~unique, labeller = "label_both") +
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

ggsave(plot = df_graph, paste0(graphs,"ctree.pdf"), height = 8, width = 10)
