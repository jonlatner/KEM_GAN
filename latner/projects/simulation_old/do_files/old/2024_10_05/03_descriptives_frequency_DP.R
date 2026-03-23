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
graphs = "graphs/"

setwd(main_dir)

#functions
options(scipen=999) 

# Set seed for reproducibility
my.seed = 1237
set.seed(my.seed)


# load original data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))
df_ods$combine <- paste(df_ods$var1, df_ods$var2, df_ods$var3, df_ods$var4, sep = "")
df_ods <- df_ods %>%
  select(-matches("var"))

df_ods <- as.data.frame(table(df_ods)) %>%
  rename(value=Freq) %>%
  mutate(type = "original")

# Generate laplace noise ---- 

epsilon <- c(.01,.1,.25,.5)
# epsilon <- c(.01)
df_frequency_sds <- data.frame()
df_frequency_ods <- data.frame()

for (e in epsilon) {
  for (c in 1:100) {
    # create seed
    my.seed = my.seed + 1
    
    # Parameters for the Laplace distribution
    location <- 0  # Mean (location parameter)
    sensitivity <- 1 # this is always 1 in integer counting queries (if integers were 2,4,6,etc., then sensitivity would be 2)
    b <- sensitivity/e # scale parameter
    
    # Generate Laplace noise for each value in n
    laplace_noise <- rlaplace(nrow(df_ods), location, b)
    
    # Add the noise to the n column
    sds <- df_ods
    sds$value <- sds$value + laplace_noise
    sds <- sds %>%
      mutate(type = "synthetic",
             dp = e)
    df_frequency_sds <- rbind(df_frequency_sds,sds)
    
  }
  
  # histogram for original data
  ods <- df_ods
  ods$dp = e
  df_frequency_ods <- rbind(df_frequency_ods,ods)
  
}

# Compare histogram ----

df_graph <- 
  ggplot() +
  geom_bar(data = df_frequency_ods, aes(x = combine, y = value, fill = type), stat = "identity") +
  geom_boxplot(data = df_frequency_sds, aes(x = combine, y = value, fill = type), alpha = .2) +
  facet_wrap(~dp) +
  scale_y_continuous(limits = c(-10,100), breaks = seq(-10,100,10)) +
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

ggsave(df_graph, filename = paste0(graphs,"graph_dp_compare_histogram_100.pdf"), height = 4, width = 10, units = "in")
