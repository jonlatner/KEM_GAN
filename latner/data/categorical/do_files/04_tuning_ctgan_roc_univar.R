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
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/drechsler_latner_2023/simulation_data/categorical/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/ctgan/"
tables = "tables/ctgan/"

#functions
options(scipen=999) 

# Create fake synthetic data ----

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))
df_ods <- df_ods %>%
  mutate_if(is.character, as.factor)
copies <- c(1, 5, 10)
for (c in copies) {
  df_synds <- syn(df_ods, m = c)
  saveRDS(df_synds, paste0(data_files,"synthetic/synds_",c,".rds"))
}

# Load data ----

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))
df_ods[df_ods == ""] <- NA

df_ods <- df_ods %>%
  mutate_if(is.character, as.factor) 

# Load synthetic data ----

df_synds_1 <- readRDS(paste0(data_files,"synthetic/synds_1.rds"))
df_synds_5 <- readRDS(paste0(data_files,"synthetic/synds_5.rds"))
df_synds_10 <- readRDS(paste0(data_files,"synthetic/synds_10.rds"))

df_univ = data.frame()

epochs = c(300, 600, 900)
discriminator = c(1, 5, 10)
batch = c(500, 1000)
frequency = c("True", "False")
copies = c(5, 10)
for (c in copies) {
  sds_list <- get(paste0("df_synds_",c))
  for (e in epochs) {
    for (d in discriminator) {
      for (b in batch) {
        for (f in frequency) {
          for (j in 1:c) {
            sds <- read.csv(paste0(synthetic_data,"sds_ctgan_tuning_e_",e,"_d_",d,"_b_",b,"_f_",f,"_m_",c,"_n_",j,".csv"))
            sds[sds == ""] <- NA
            sds <- sds %>%
              mutate_if(is.character, as.factor)
            sds_list$syn[[j]] <- sds
          }
          df_sds <- compare(sds_list, df_ods)
          df_sds <- data.frame(df_sds$tables) %>%
            rownames_to_column(var = "data") %>%
            pivot_longer(cols = starts_with(names(df_ods))) %>%
            rename(prop = value) %>%
            mutate(variable = str_split_fixed(name, "\\.", n = 2)[, 1],
                   value = str_split_fixed(name, "\\.", n = 2)[, 2]) %>%
            select(-name) %>%
            pivot_wider(names_from = data, values_from = prop, names_prefix = "prop.") %>%
            group_by(variable) %>%
            mutate(max = pmax(prop.observed, prop.synthetic),
                   min = pmin(prop.observed, prop.synthetic)) %>%
            summarize(roc_univar = mean(min/max)) %>%
            ungroup()
          df_sds$epochs = e
          df_sds$discriminator =  d
          df_sds$frequency =  f
          df_sds$batch =  b
          df_sds$copies = c
          df_univ <- rbind(df_univ,df_sds)
        }
      }
    }
  }
}

copies = c(1)
for (c in copies) {
  sds_list <- get(paste0("df_synds_",c))
  for (e in epochs) {
    for (d in discriminator) {
      for (b in batch) {
        for (f in frequency) {
            sds <- read.csv(paste0(synthetic_data,"sds_ctgan_tuning_e_",e,"_d_",d,"_b_",b,"_f_",f,"_m_",c,"_n_1.csv"))
            sds[sds == ""] <- NA
            sds <- sds %>%
              mutate_if(is.character, as.factor)
            sds_list$syn <- sds
          df_sds <- compare(sds_list, df_ods)
          df_sds <- data.frame(df_sds$tables) %>%
            rownames_to_column(var = "data") %>%
            pivot_longer(cols = starts_with(names(df_ods))) %>%
            rename(prop = value) %>%
            mutate(variable = str_split_fixed(name, "\\.", n = 2)[, 1],
                   value = str_split_fixed(name, "\\.", n = 2)[, 2]) %>%
            select(-name) %>%
            pivot_wider(names_from = data, values_from = prop, names_prefix = "prop.") %>%
            group_by(variable) %>%
            mutate(max = pmax(prop.observed, prop.synthetic),
                   min = pmin(prop.observed, prop.synthetic)) %>%
            summarize(roc_univar = mean(min/max)) %>%
            ungroup()
          df_sds$epochs = e
          df_sds$discriminator =  d
          df_sds$frequency =  f
          df_sds$batch =  b
          df_sds$copies = c
          df_univ <- rbind(df_univ,df_sds)
        }
      }
    }
  }
}

## Clean table 

head(df_univ, 10)

# df_univ$copies <- NULL

data_for_clustering <- df_univ %>%
  group_by(epochs, discriminator, batch, frequency, copies) %>%
  summarise(roc_univar = mean(roc_univar)) %>%
  ungroup() %>%
  mutate(epochs = factor(epochs),
         discriminator = as.factor(discriminator),
         frequency = as.factor(frequency),
         batch = as.factor(batch),
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

ggsave(plot = df_graph, paste0(graphs,"graph_ctgan_roc_univar_raw.pdf"), height = 4, width = 6)

## Linear model

lm_model <- lm(roc_univar ~ epochs + discriminator + batch + frequency + copies, data = data_for_clustering)
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

ggsave(plot = df_graph, paste0(graphs,"graph_ctgan_roc_univar.pdf"), height = 4, width = 6)

## Linear model by copies

# write.csv(df_univ, paste0(tables,"utility_roc_univar.csv"), row.names=FALSE)

# Create an empty list to store the results
tidy_model_data_list <- list()

# Define the values of 'copies' you want to analyze
copies_values <- c(1, 5, 10)

# Loop through each value of 'copies' and fit the linear model
for (c in copies_values) {
  lm_model <- lm(roc_univar ~ epochs + discriminator + batch + frequency, data = subset(data_for_clustering, copies == c))
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

ggsave(plot = df_graph, paste0(graphs,"graph_ctgan_roc_univar_facet.pdf"), height = 9, width = 6)

## Summary table 

head(data_for_clustering, 10)
data_for_clustering$n <- NULL
write.csv(data_for_clustering, paste0(tables,"utility_roc_univar.csv"), row.names=FALSE)

