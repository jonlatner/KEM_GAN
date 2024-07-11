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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/datasynthesizer/"
tables = "tables/datasynthesizer/"

setwd(main_dir)


# Load synthetic data ----

data <- c("sd2011_clean_small")
parents = c(0, 1, 2, 3)
privacy = c(0)
copies = c(5)

df_comparison <- data.frame()

for (c in copies) {
  for (d in data) {
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
    df_ods <- read.csv(paste0(original_data,d,".csv")) # load original data
    for (e in privacy) {
      for (k in parents) {
        for (j in 1:c) {
          sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_",d,"_k_",k,"_e_",e,"_m_",c,"_n_",j,".csv"))
          sds[sds == ""] <- NA
          sds <- sds %>%
            mutate_if(is.character, as.factor)
          sds_list$syn[[j]] <- sds  # use when m>1
          # sds_list$syn <- sds # use when m==1
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
        df_compare$parents = k
        df_compare$privacy = e
        df_compare$dataset = d
        
        df_comparison <- rbind(df_comparison,df_compare)
      }
    }
  }
}

# Graph ----

# this function rounds .5 up to the nearest whole number
round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

# this cleans the value levels so that months are whole numbers and other values remain the same
df_graph_data <- df_comparison %>%
  filter(variables %in% c("age","income","sex","wkabdur","mmarital","mmarr")) %>%
  # filter(variables %in% c("mmarr","msepdiv")) %>%
  mutate(drop = ifelse((variables == "mmarr" | variables == "msepdiv") & pct == 0, yes = 1, no = 0)) %>%
  filter(drop == 0) %>%
  mutate(value2 = ifelse((variables == "mmarr" | variables == "msepdiv"), yes = round2(as.numeric(as.character(value)),0), no = NA)) %>%
  mutate(value = ifelse(!is.na(value2), yes = as.character(value2), no = as.character(value))) %>%
  filter(dataset == "sd2011_clean_small") 

df_graph_data <- df_comparison %>%
  filter(variables %in% c("income","sex","edu")) %>%
  filter(dataset == "sd2011_clean_small") 

df_graph_data$value <- factor(as.character(df_graph_data$value), levels = str_sort(unique(df_graph_data$value), numeric = TRUE))
df_graph_data$value <- fct_relevel(df_graph_data$value, "NA", after = Inf)


df_graph <- ggplot(df_graph_data, aes(x = value, y = pct, fill = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_nested(parents ~ variables, scales = "free", labeller = labeller(.rows = label_both)) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_frequency_optimize_variables.pdf"), height = 4, width = 6)

# Graph ----

df_graph_data <- df_comparison %>%
  filter(variables %in% c("height","sex","edu","weight","income")) %>%
  # filter(data == "synthetic") %>%
  mutate(parents = ifelse(data == "observed", yes = "observed", no = parents)) %>%
  filter(dataset == "sd2011_clean_small") 

df_graph_data$value <- factor(as.character(df_graph_data$value), levels = str_sort(unique(df_graph_data$value), numeric = TRUE))
df_graph_data$parents <- factor(as.character(df_graph_data$parents), levels = str_sort(unique(df_graph_data$parents), numeric = TRUE))
df_graph_data$value <- fct_relevel(df_graph_data$value, "NA", after = Inf)
df_graph_data

df_graph <- ggplot(df_graph_data, aes(x = value, y = pct, fill = parents)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_nested( ~ variables, scales = "free", labeller = labeller(.rows = label_both)) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        text = element_text(size = 6),
        legend.margin = margin(t = -50),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_frequency_optimize_variables_parents.pdf"), height = 4, width = 10)



