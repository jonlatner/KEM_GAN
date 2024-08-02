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

setwd(main_dir)

#functions
options(scipen=999) 

# load original data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))

# Graph frequency by id ----

df_frequency <- df_ods
df_frequency$combine <- paste(df_frequency$var1, df_frequency$var2, df_frequency$var3, df_frequency$var4, sep = "")
df_frequency <- df_frequency %>%
  select(-matches("var"))

df_frequency <- as.data.frame(table(df_frequency)) %>%
  rename(value=Freq) %>%
  mutate(type = "original",
         n = 1)

# Generate laplace noise ---- 

epsilon <- c(0.05,.1,.25,.5,1,5)
df_compare <- data.frame()
for (e in epsilon) {
  for (c in 1:100) {
    ods <- df_frequency %>%
      mutate(dp = e)
    
    # Parameters for the Laplace distribution
    location <- 0  # Mean (location parameter)
    sensitivity <- 1 # this is always 1 in integer counting queries (if integers were 2,4,6,etc., then sensitivity would be 2)
    b <- sensitivity/e # scale parameter
    
    # Generate Laplace noise for each value in n
    laplace_noise <- rlaplace(nrow(df_frequency), location, b)
    
    # Add the noise to the n column
    sds <- df_frequency
    sds$value <- round(sds$value + laplace_noise,0)
    sds <- sds %>%
      mutate(type = "synthetic",
             dp = e,
             n = c)
    df_compare <- rbind(df_compare,ods,sds)
  }
}

# Plot the percent frequency histogram by levels of differential privacy ----

df_graph_sds <- df_compare %>%
  filter(type == "synthetic") %>%
  arrange(type,dp,n) %>%
  mutate(dp = as.factor(dp))

df_graph_ods <- df_compare %>%
  filter(type == "original") %>%
  mutate(dp = as.factor(dp))
df_graph_ods <- unique(df_graph_ods)

ggplot() +
  geom_boxplot(position = position_dodge(width=0.9), aes(x = combine, y = value, fill = type), data = df_graph_sds, alpha = .2) +
  geom_bar(data = df_graph_ods, aes(x = combine, y = value, fill = type), position = position_dodge(width=0.9), stat = "identity", alpha = .2) +
  facet_wrap(~dp, labeller = "label_both") +
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

# Same, but for unique value ----

df_graph <- df_compare %>%
  filter(combine == "1111",
         type == "synthetic") %>%
  arrange(type,dp,n) %>%
  mutate(dp = as.factor(dp))

ggplot(df_graph, aes(x = dp, y = value)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
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
