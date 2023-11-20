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
library(broom)
library(tidyverse)
library(ggh4x) # facet_nested
library(synthpop)

# FOLDERS - ADAPT THIS PATHWAY
# main_dir = "N:/Ablagen/D01700-KEM/Latner/little_etal_2021/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/drechsler_latner_2023/simulation_data/continuous/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/datasynthesizer/"
tables = "tables/datasynthesizer/"

#functions
source("/Users/jonathanlatner/Google Drive/My Drive/IAB/little_etal_2021/do_files/R/roc_function.R")
options(scipen=999) 

# Load data ----

# original data

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))

# prepare data ----

#select categorical variables
continuous_vars <- sapply(df_ods, function(x) is.numeric(x))
continuous_var_names <- names(df_ods[continuous_vars])

# Ratio of estimates (univariate) ----

# synthetic data

df_univ <- data.frame(
  privacy = character(),
  parents = numeric(),
  copies = numeric(),
  variable = character(),
  roc_univar = numeric()
)

parents = c(0, 1, 2)
privacy = c(0, 0.1, 1, 5, 10, 20, 30)
copies = c(1, 5, 10)

for (e in privacy) {
  for (k in parents) {
    for (c in copies) {
      df_sds = data.frame()
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_tuning_e_",e,"_k_",k,"_m_",c,"_n_",j,".csv"))
        df_sds <- rbind(df_sds,sds)
      }
      count = 1
      df_ods_binned <- df_ods
      for (col_name in continuous_var_names) {
        bins <- seq(min(df_ods_binned[[col_name]]), max(df_ods_binned[[col_name]]), length.out = 21) # 21 points to get 20 bins
        df_ods_binned[[col_name]] <- cut(df_ods_binned[[col_name]], breaks = bins)
        df_sds[[col_name]] <- cut(df_sds[[col_name]], breaks = bins)
        
        roc_univ <- roc_univariate(df_ods_binned, df_sds, count)
        
        df_univ <- rbind(df_univ, 
                         data.frame(
                           privacy = e,
                           parents = k,
                           copies = c,
                           variable = col_name,
                           roc_univar = roc_univ
                         ))
        
        count = count + 1
      }
    }
  }
}

## Clean table 

head(df_univ, 10)

# df_univ$copies <- NULL

data_for_clustering <- df_univ %>%
  group_by(privacy, parents, copies) %>%
  summarise(roc_univar = mean(roc_univar)) %>%
  ungroup() %>%
  mutate(privacy = factor(privacy),
         parents = as.factor(parents),
         copies = as.factor(copies),
  ) %>%
  arrange(roc_univar) %>%
  mutate(n = row_number())

head(data_for_clustering, 10)

## Graph

df_graph <- ggplot(data_for_clustering, aes(y = roc_univar, x = n)) +
  geom_point() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_datasynthesizer_roc_univar_raw.pdf"), height = 4, width = 6)

## Linear model

lm_model <- lm(roc_univar ~ privacy + parents + copies, data = data_for_clustering)
summary(lm_model)

tidy_model_data <- tidy(lm_model) %>%
  filter(term!="(Intercept)")

## Graph

df_graph <- ggplot(tidy_model_data, aes(y = term, x = estimate, 
                                        xmin = estimate - (1.96 * std.error), xmax = estimate + (1.96 * std.error))) +
  geom_point() +
  geom_errorbar() +
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

ggsave(plot = df_graph, paste0(graphs,"graph_datasynthesizer_roc_univar.pdf"), height = 4, width = 6)

## Linear model by copies

# Create an empty list to store the results
tidy_model_data_list <- list()

# Define the values of 'copies' you want to analyze
copies_values <- c(1, 5, 10)

# Loop through each value of 'copies' and fit the linear model
for (c in copies_values) {
  lm_model <- lm(roc_univar ~ privacy + parents, data = subset(data_for_clustering, copies == c))
  tidy_model_data <- tidy(lm_model) %>% filter(term != "(Intercept)") %>% mutate(copies = c)
  tidy_model_data_list <- rbind(tidy_model_data_list,tidy_model_data)
}

## Graph with facet wrap

df_graph <- ggplot(tidy_model_data_list, aes(y = term, x = estimate, 
                                             xmin = estimate - (1.96 * std.error), xmax = estimate + (1.96 * std.error))) +
  facet_wrap(~copies,labeller = (.cols = label_both), ncol = 1) +
  geom_point() +
  geom_errorbar() +
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
ggsave(plot = df_graph, paste0(graphs,"graph_datasynthesizer_roc_univar_facet.pdf"), height = 9, width = 6)

## Summary table 

head(data_for_clustering, 10)
data_for_clustering$n <- NULL
write.csv(data_for_clustering, paste0(tables,"utility_roc_univar.csv"), row.names=FALSE)

