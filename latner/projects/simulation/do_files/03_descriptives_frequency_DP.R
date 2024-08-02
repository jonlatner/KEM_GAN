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

df_frequency <- as.data.frame(table(df_frequency)) %>%
  rename(value=Freq) %>%
  mutate(type = "original")

ggplot(df_frequency, aes(x = combine, y = value)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ylab("Frequency") +
  theme_bw() +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  geom_text(aes(label = value),vjust = -0.5, size = 4) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

# Generate laplace noise ---- 

epsilon <- c(.01,.1,.25,.5,1,5)

df_compare <- data.frame()
for (e in epsilon) {
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
  sds$value <- sds$value + laplace_noise
  sds <- sds %>%
    mutate(type = "synthetic",
           dp = e)
  df_compare <- rbind(df_compare,ods,sds)
}

# Plot the percent frequency histogram with differential privacy ----

df_graph <- df_compare %>%
  mutate(text = ifelse(combine == "1111", yes = value, no = NA))
ggplot(df_graph, aes(x=combine, y=value, fill = type)) +
  facet_wrap2(~dp, labeller = label_both)+
  geom_bar(stat="identity", position = position_dodge(width=.9)) +
  theme_bw() +
  geom_text(aes(label=round(text,0), vjust = ifelse(text > 0, yes = -0.5, no = 2)), position = position_dodge(width=.9), size = 3) +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(2, "cm"),
  )

df_graph <- df_compare %>%
  filter(combine == "1111")
ggplot(df_graph, aes(x=combine, y=value, fill = as.factor(dp))) +
  # facet_wrap2(~dp, labeller = label_both)+
  geom_bar(stat="identity", position = position_dodge(width=.9)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(2, "cm"),
  )
