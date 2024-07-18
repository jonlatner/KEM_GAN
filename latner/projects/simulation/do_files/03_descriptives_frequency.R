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
original_data = "data_files/original/"
synthpop_data = "data_files/synthetic/synthpop/"
datasynthesizer_data = "data_files/synthetic/datasynthesizer/"

setwd(main_dir)

#functions
options(scipen=999) 

# load original data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))

# Graph frequency by variable ----

# Reshape the data for plotting
df_ods_long <- df_ods %>%
  pivot_longer(cols = starts_with("var"), names_to = "variable", values_to = "value") %>%
  mutate(value = as.factor(value)) %>%
  group_by(variable,value) %>%
  tally() %>%
  group_by(variable) %>%
  mutate(total = sum(n),
         pct = n/total) %>%
  ungroup()

df_ods_long

# Plot using facet_wrap
ggplot(df_ods_long, aes(x = value, y=n)) +
  geom_bar(color = "black", stat = 'identity') +
  geom_text(aes(label = n),vjust = -0.5, size = 4) +
  facet_wrap(~variable, scales = "free") +
  theme_bw() +
  scale_y_continuous(limits = c(0,700), breaks = seq(0,700,100)) +
  theme(panel.grid.minor = element_blank(), 
        axis.title = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

# Graph frequency by id ----

df_frequency <- df_ods
df_frequency$combine <- paste(df_frequency$var1, df_frequency$var2, df_frequency$var3, df_frequency$var4, sep = "")
df_frequency <- df_frequency %>%
  select(-matches("var"))

df_frequency <- as.data.frame(table(df_frequency))
df_frequency$pct <- (df_frequency$Freq / nrow(df_ods)) * 100


ggplot(df_frequency, aes(x = combine, y = Freq)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ylab("Frequency") +
  theme_bw() +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  geom_text(aes(label = Freq),vjust = -0.5, size = 4) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )
