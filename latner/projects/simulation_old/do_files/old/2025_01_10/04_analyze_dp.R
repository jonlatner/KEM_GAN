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
my.seed = 1234
set.seed(my.seed)


# load original data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))

# Graph frequency by id ----

df_frequency <- df_ods
df_frequency$combine <- paste(df_frequency$var1, df_frequency$var2, df_frequency$var3, df_frequency$var4, sep = "")
df_frequency <- df_frequency %>%
  select(-matches("var"))

df_frequency_ods <- as.data.frame(table(df_frequency)) %>%
  rename(value=Freq) 

# Generate laplace noise ---- 

epsilon <- c(.01,.1,.25,.5)
df_frequency_sds <- data.frame()

for (c in 1:100) {
  for (e in epsilon) {
    ods <- df_frequency_ods %>%
      mutate(epsilon = e)
    
    # Parameters for the Laplace distribution
    location <- 0  # Mean (location parameter)
    sensitivity <- 1 # this is always 1 in integer counting queries (if integers were 2,4,6,etc., then sensitivity would be 2)
    b <- sensitivity/e # scale parameter
    
    # Generate Laplace noise for each value in n
    laplace_noise <- rlaplace(nrow(df_frequency_ods), location, b)
    
    # Add the noise to the n column
    sds <- df_frequency_ods
    sds$Freq <- sds$value + laplace_noise
    sds <- sds %>%
      mutate(type = "synthetic",
             dp = e,
             m = c)
    df_frequency_sds <- rbind(df_frequency_sds,sds)
  }
}

# Graph ----

df_test <- data.frame()
for (e in epsilon) {
  df_ods_frequency <- df_ods
  df_ods_frequency$combine <- paste(df_ods_frequency$var1, df_ods_frequency$var2, df_ods_frequency$var3, df_ods_frequency$var4, sep = "")
  df_ods_frequency <- df_ods_frequency %>%
    select(-matches("var"))
  df_ods_frequency <- as.data.frame(table(df_ods_frequency))
  df_ods_frequency$type <- "original"
  df_ods_frequency$dp <- e
  df_test <- rbind(df_test,df_ods_frequency)
}
df_ods_frequency <- df_test
df_graph_ods <- df_ods_frequency

df_graph_sds <- df_frequency_sds 

df_graph <- 
  ggplot() +
  geom_bar(data = df_graph_ods, aes(x = combine, y = Freq, fill = type), stat = "identity") +
  geom_boxplot(data = df_graph_sds, aes(x = combine, y = Freq, fill = type), alpha = .2) +
  facet_wrap(~dp) +
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

ggsave(df_graph, filename = paste0(graphs,"graph_dp_laplace_compare_histogram_10.pdf"), height = 4, width = 6, units = "in")
ggsave(df_graph, filename = paste0(graphs,"graph_dp_laplace_compare_histogram_10_v2.pdf"), height = 4, width = 10, units = "in")

