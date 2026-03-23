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

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
graphs = "graphs/"
original_data = "data_files/original/"
synthpop_data = "data_files/synthetic/synthpop/"
datasynthesizer_data = "data_files/synthetic/datasynthesizer/"

setwd(main_dir)

#functions
options(scipen=999) 

# Set seed for reproducibility
my.seed = 1237

# Load original data ----
ods <- read.csv(paste0(original_data,"simulated.csv"))

# Create fake synthetic data ----

sds <- syn(ods[sample(1:1000),], m = 1, seed = my.seed)
# sds <- syn(ods, m = 1, seed = my.seed)
sds <- sds$syn

# Compare frequency ----

df_ods <- ods
df_sds <- sds

df_sds$type <- "synthetic"
df_ods$type <- "original"

# Compare histogram ----

df_ods_frequency <- ods
df_ods_frequency$combine <- paste(df_ods_frequency$var1, df_ods_frequency$var2, df_ods_frequency$var3, df_ods_frequency$var4, sep = "")
df_ods_frequency <- df_ods_frequency %>%
  select(-matches("var"))
df_ods_frequency <- as.data.frame(table(df_ods_frequency))
df_ods_frequency$pct <- (df_ods_frequency$Freq / nrow(df_ods)) * 100
df_ods_frequency$type <- "original"

df_sds_frequency <- sds
df_sds_frequency$combine <- paste(df_sds_frequency$var1, df_sds_frequency$var2, df_sds_frequency$var3, df_sds_frequency$var4, sep = "")
df_sds_frequency <- df_sds_frequency %>%
  select(-matches("var"))
df_sds_frequency <- as.data.frame(table(df_sds_frequency))
df_sds_frequency$pct <- (df_sds_frequency$Freq / nrow(df_sds)) * 100
df_sds_frequency$type <- "original"
df_sds_frequency$type <- "synthetic"

df_combine <- rbind(df_ods_frequency,df_sds_frequency)

# Plot using facet_wrap
p <- ggplot(df_combine, aes(x = combine, y=Freq, fill = type)) +
  geom_bar(stat = 'identity',position = position_dodge(.9)) +
  geom_text(aes(label = Freq),vjust = -0.5, size = 4,position = position_dodge(.9)) +
  theme_bw() +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

p

