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
library(readr)
library(ggh4x)

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

df_ods <- as.data.frame(table(df_ods))
df_ods$synthesizer <- "original"
df_ods$combine <- paste(df_ods$var1, df_ods$var2, df_ods$var3, df_ods$var4, sep = "")
df_ods <- df_ods %>%
  select(-matches("var"))
df_ods

df_test_original <- data.frame()
privacy = c(0,0.1,0.25,0.5,0.75,1,10,25,50)
for (e in privacy) {
  test = df_ods %>%
    mutate(epsilon = e)
  df_test_original <- rbind(df_test_original,test)
}

# Load synthetic data from synthpop ----

df_synthpop <- read_csv(paste0(synthpop_data,"synthpop.csv"))
df_synthpop$synthesizer <- "synthpop"
df_synthpop

df_test_synthpop <- data.frame()
privacy = c(0,0.1,0.25,0.5,0.75,1,10,25,50)
for (e in privacy) {
  test = df_synthpop %>%
    mutate(epsilon = e)
  df_test_synthpop <- rbind(df_test_synthpop,test)
}

# Load synthetic data from datasynthesizer ----

df_datasynthesizer <- read_csv(paste0(datasynthesizer_data,"datasynthesizer.csv"))
df_datasynthesizer$synthesizer <- "datasynthesizer"

# Graph frequency ----

df_compare <- bind_rows(df_test_original,df_datasynthesizer,df_test_synthpop) 
df_compare

ggplot(df_compare, aes(x = combine, y = Freq, fill = synthesizer)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  facet_wrap2(~epsilon,labeller = label_both) +
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

# Graph frequency ----

df_compare <- bind_rows(df_test_original,df_datasynthesizer,df_test_synthpop) %>%
  filter(combine == "1111")

ggplot(df_compare, aes(x = synthesizer, y = Freq)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  facet_wrap2(~epsilon,labeller = label_both) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_compare <- df_compare %>%
  mutate(Freq = ifelse(combine == 1111, yes = round(Freq,1), no = NA))
+
  geom_text(aes(label = label2), hjust = -0.5, size = 3, position = position_dodge(width=.9), angle = 90) +
  