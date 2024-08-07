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

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/ctgan/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/ctgan/"
tables = "tables/ctgan/"

setwd(main_dir)

# Load original data ----

data <- c("sd2011_small","sd2011_small_complete")
copies = c(1)

df_comparison <- data.frame()

for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
        sds <- read.csv(paste0(synthetic_data,"sds_ctgan_data_",d,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        # sds_list$syn[[j]] <- sds  # use when m>1
        sds_list$syn <- sds # use when m==1
        
        df_temp <- compare(sds_list, df_ods) 
        df_compare <- data.frame(df_temp$tables) %>%
          rownames_to_column(var = "data") %>%
          pivot_longer(cols = starts_with(names(df_ods))) %>%
          rename(pct = value) %>%
          separate(name, into = c("variables", "value"), sep = "\\.\\.|\\.", remove = FALSE) %>%
          mutate(contains_double_dot = if_else(str_detect(name, fixed("..")), 1, 0),
                 value_new = as.numeric(value),
                 value_new = as.character(if_else(contains_double_dot == 1, -1*value_new, value_new)),
                 value_new = if_else(is.na(value_new), value, value_new),
                 value_new = if_else(value_new == "miss", NA, value_new),
          ) %>%
          select(-name,-contains_double_dot,-value) %>%
          rename(value=value_new)
        df_compare$dataset = d
        
        df_comparison <- rbind(df_comparison,df_compare)
  }
}

# Graph single variable ----

observed <- data.frame(with(df_ods,table(income)))
names(observed)[1:2] <- c("value", "freq")
observed$data <- "observed"

summary(df_ods$income)
summary(sds$income)

synthetic <- data.frame(with(sds,table(income)))
names(synthetic)[1:2] <- c("value", "freq")
synthetic$data <- "synthetic"

df_compare <- rbind(observed,synthetic)%>%
  mutate(value = as.numeric(as.character(value))) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() 

head(df_compare,10)

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data, color = data, group = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

# Graph ----

# this cleans the value levels so that months are whole numbers and other values remain the same
df_graph_data <- df_comparison 

df_graph <- ggplot(df_graph_data, aes(x = value, y = pct, fill = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_grid(dataset ~ variables, scales = "free", labeller = labeller(.rows = label_both)) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

# ggsave(plot = df_graph, paste0(graphs,"ctgan_frequency_optimize_variables.pdf"), height = 4, width = 6)
