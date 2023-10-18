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
library(xtable)

# FOLDERS - ADAPT THIS PATHWAY
# main_dir = "N:/Ablagen/D01700-KEM/Latner/little_etal_2021/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/drechsler_latner_2023/simulation_data/categorical/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/ctgan/"
tables = "tables/ctgan/"

#functions
options(scipen=999) 

# Load utility baseline data ----

df_univar <- read.csv(paste0(tables,"utility_roc_univar.csv")) %>%
  rename(value = roc_univar) %>%
  mutate(epochs = factor(epochs),
         discriminator = as.factor(discriminator),
         frequency = as.factor(frequency),
         batch = as.factor(batch),
         copies = as.factor(copies),
         dv = "univar")
         
df_bivar <- read.csv(paste0(tables,"utility_roc_bivar.csv")) %>%
  rename(value = roc_bivar) %>%
  mutate(epochs = factor(epochs),
         discriminator = as.factor(discriminator),
         frequency = as.factor(frequency),
         batch = as.factor(batch),
         copies = as.factor(copies),
         dv = "bivar")

df_utility <- rbind(df_univar, df_bivar)
         
## Linear model by utility ----

tidy_model_data_list <- list()
dep_var <- c("univar", "bivar")

# Loop through each value of 'copies' and fit the linear model
for (dv in dep_var) {
  df_test <- df_utility %>%
    filter(dv==dv) %>%
    select(-dv)
  lm_model <- lm(value ~ ., data = df_test)
  tidy_model_data <- tidy(lm_model) %>% 
    filter(term != "(Intercept)") %>% 
    mutate(dv = dv)
  tidy_model_data_list <- rbind(tidy_model_data_list,tidy_model_data)
}

## Graph

df_graph <- ggplot(tidy_model_data_list, aes(y = term, x = estimate, 
                                        xmin = estimate - (1.96 * std.error), xmax = estimate + (1.96 * std.error))) +
  geom_point() +
  geom_errorbar() +
  facet_wrap(~dv, labeller = labeller(.cols = label_both)) +
  labs(y = "Variables", x = "Estimate coefficeint with 95% CI") +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "solid", color = "red") +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_ctgan_roe.pdf"), height = 4, width = 10)

## Linear model by utility by copies ----

# Create an empty list to store the results
tidy_model_data_list <- list()
copies <- c(1, 5, 10)
dep_var <- c("univar", "bivar")

# Loop through each value of 'copies' and fit the linear model
for (c in copies) {
  for (dv in dep_var) {
    df_test <- df_utility %>%
      filter(dv==dv&copies==c) %>%
      select(-dv,-copies)
    lm_model <- lm(value ~ ., data = df_test)
    tidy_model_data <- tidy(lm_model) %>% 
      filter(term != "(Intercept)") %>% 
      mutate(dv = dv,
             copies = c)
    tidy_model_data_list <- rbind(tidy_model_data_list,tidy_model_data)
  }
}

# Graph

df_graph <- ggplot(tidy_model_data_list, aes(y = term, x = estimate, 
                                             xmin = estimate - (1.96 * std.error), xmax = estimate + (1.96 * std.error))) +
  geom_point() +
  geom_errorbar() +
  facet_grid(dv~copies, labeller = labeller(.cols = label_both, .rows = label_both)) +
  labs(y = "Variables", x = "Estimate coefficeint with 95% CI") +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "solid", color = "red") +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_ctgan_roe_copies.pdf"), height = 4, width = 10)
