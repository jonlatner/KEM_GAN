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

df_ods <- read.csv(paste0(original_data,"simulated.csv"))

# Loop ----

df_frequency <- data.frame()
for (c in 1:100) {

    # create seed
    my.seed = my.seed + 1
    
    # Create fake synthetic data
    sds <- syn(df_ods, m = 1, seed = my.seed, cart.minbucket = 75)
    sds <- sds$syn
    
    # Create a frequency table for synthetic data
    
    sds$combine <- paste(sds$var1, sds$var2, sds$var3, sds$var4, sep = "")
    sds <- sds %>%
      select(-matches("var"))
    df_sds_frequency <- as.data.frame(table(sds))
    df_sds_frequency$type <- "synthetic"
    df_sds_frequency$n <- c

    # Combine
    df_frequency <- rbind(df_frequency,df_sds_frequency)
}

df_frequency

# Save data ----

write.csv(df_frequency, paste0(synthetic_data,"synthetic_cart_modified_100_mb.csv"), row.names = FALSE)

# Compare histogram ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))
df_frequency <- read_csv(paste0(synthetic_data,"synthetic_cart_modified_100_mb.csv"))

df_graph_sds <- df_frequency 

df_ods_frequency <- df_ods
df_ods_frequency$combine <- paste(df_ods_frequency$var1, df_ods_frequency$var2, df_ods_frequency$var3, df_ods_frequency$var4, sep = "")
df_ods_frequency <- df_ods_frequency %>%
  select(-matches("var"))
df_ods_frequency <- as.data.frame(table(df_ods_frequency))
df_ods_frequency$pct <- (df_ods_frequency$Freq / nrow(df_ods)) * 100
df_ods_frequency$type <- "original"

df_graph_ods <- df_ods_frequency

df_graph <- 
  ggplot() +
  geom_bar(data = df_graph_ods, aes(x = combine, y = Freq, fill = type), stat = "identity") +
  geom_boxplot(data = df_graph_sds, aes(x = combine, y = Freq, fill = type), alpha = .2) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(df_graph, filename = paste0(graphs,"graph_cart_modified_histogram_compare_100.pdf"), height = 4, width = 10, units = "in")
