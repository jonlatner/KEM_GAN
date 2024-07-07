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
library(ggh4x) # facet_nested

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"

setwd(main_dir)

#functions
options(scipen=999) 

# load original data ----

df_ods <- read.csv(paste0(original_data,"sd2011.csv"))

# load synthetic data ----

copies=2
data="sd2011"
df_sds <- data.frame()
for (j in 1:copies) {
  sds <- read.csv(paste0(synthetic_data,"sds_synthpop_",data,"_m_",copies,"_n_",j,".csv"))
  sds[sds == ""] <- NA
  sds <- sds %>%
    mutate_if(is.character, as.factor)
  df_sds <- rbind(df_sds,sds)
}
rm(sds)

# compare data ----

df_comparison <- data.frame()

df_temp <- compare(df_sds, df_ods) 

df_compare <- data.frame(df_temp$tables) %>%
  rownames_to_column(var = "data") %>%
  pivot_longer(cols = starts_with(names(df_ods))) %>%
  rename(pct = value) %>%
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

# Graph ----

df_graph_data <- df_compare 

df_graph_data$value <- factor(as.character(df_graph_data$value), levels = str_sort(unique(df_graph_data$value), numeric = TRUE))
df_graph_data$value <- fct_relevel(df_graph_data$value, "NA", after = Inf)

df_graph <- ggplot(df_graph_data, aes(x = value, y = pct, fill = data, color = data, group = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_grid( ~ variables, scales = "free", labeller = labeller(.rows = label_both)) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

