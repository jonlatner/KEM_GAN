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
library(synthpop)
library(ggh4x) # facet_nested
library(ggcorrplot)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
graphs = "graphs/"
tables = "tables/synthpop/"

setwd(main_dir)

# Load original data ----

data <- c("adult","grid","gridr","sd2011_small")
data <- c("sd2011_small")
for (d in data) {
  df_ods <- read.csv(paste0(original_data,d,".csv"))
}

# Load synthetic data ----

copies = c(1)

df_comparison <- data.frame()


for (c in copies) {
  for (d in data) {
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"sds_synthpop_",d,"_m_",c,"_n_",j,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        # sds_list$syn[[j]] <- sds  # use when m>1
        sds_list$syn <- sds # use when m==1
      }
      
      df_temp <- compare(sds_list, df_ods) 
      df_compare <- data.frame(df_temp$tables) %>%
        rownames_to_column(var = "data") %>%
        pivot_longer(cols = starts_with(names(df_ods))) %>%
        rename(pct = value) %>%
        # separate(name, into = c("variables", "value"), sep = "\\.\\.|\\.", remove = FALSE)
        separate(name, into = c("variables", "value"), sep = "\\.\\.|\\.", remove = FALSE, extra = "merge") %>%
        mutate(contains_double_dot = if_else(str_detect(name, fixed("..")), 1, 0),
               value_new = as.numeric(value),
               value_new = as.character(if_else(contains_double_dot == 1, -1*value_new, value_new)),
               value_new = if_else(is.na(value_new), value, value_new),
               value_new = if_else(value_new == "miss", NA, value_new),
        ) %>%
        select(-name,-contains_double_dot,-value) %>%
        rename(value=value_new) %>%
        mutate(value = ifelse(value=="miss.NA", yes = "NA", no = value))

      df_comparison <- rbind(df_comparison,df_compare)
  }
}

# utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "SPECKS", nperms = 3)
# utility_measure
# compare(sds_list, df_ods) 

# Graph ----

df_comparison$value <- factor(as.character(df_comparison$value), levels = str_sort(unique(df_comparison$value), numeric = TRUE))
df_comparison$value <- fct_relevel(df_comparison$value, "NA", after = Inf)
table(df_comparison$value)

# df_comparison <- df_comparison %>%
#   filter(variables=="region")

df_graph <- ggplot(df_comparison, aes(x = value, y = pct, fill = data, color = data, group = data)) +
  # geom_bar(data = subset(df_compare, data=="observed"), position = position_dodge(width = .9), stat = "identity") +
  # geom_line(data = subset(df_compare, data!="observed")) +
  # geom_line() +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_grid( ~ variables, scales = "free", labeller = labeller(.rows = label_both)) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 90,vjust=.5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

# Heatmaps ----

model.matrix(~0+., data=df_ods) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2) 

# Convert correlation matrix to long format for plotting
cor_df_ods <- as.data.frame(cor_matrix) %>%
  rownames_to_column(var = "variable") %>%
  pivot_longer(-variable, names_to = "variable2", values_to = "correlation")%>%
  mutate(type = "original")

# Calculate correlation matrix
cor_matrix <- model.matrix(~0 + ., data = sds) %>%
  cor(use = "pairwise.complete.obs")

# Convert correlation matrix to long format for plotting
cor_df_sds <- as.data.frame(cor_matrix) %>%
  rownames_to_column(var = "variable") %>%
  pivot_longer(-variable, names_to = "variable2", values_to = "correlation") %>%
  mutate(type = "synthetic")

cor_df <- rbind(cor_df_ods,cor_df_sds) %>%
  mutate(correlation = ifelse(variable==variable2, yes = 0, no = correlation))

cor_df <- rbind(cor_df_ods,cor_df_sds)

# Plot using ggplot with facets
ggplot(cor_df, aes(x = variable, y = variable2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation Plot") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ type) +
  coord_fixed()
