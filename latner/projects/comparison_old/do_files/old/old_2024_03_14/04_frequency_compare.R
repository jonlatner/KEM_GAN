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
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"

setwd(main_dir)


# this function rounds .5 up to the nearest whole number
round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

data <- c("sd2011_clean_small")

# Load synthetic data from CTGAN (based on optimized parameterization) ----

copies = c(5)
e = c(50)

df_ctgan <- data.frame()

for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"ctgan/sds_ctgan_data_",d,"_epochs_",e,"_m_",c,"_n_",j,".csv"))
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
      
      # Splitting the string
      # split_string <- strsplit(as.character(df_compare$Variable), ": S_pMSE")
      # df_compare$Variable <- sapply(split_string, `[`, 1)

      df_compare$type <- df_compare$data
      df_compare$data <- d
      df_ctgan <- rbind(df_ctgan,df_compare) %>%
        mutate(type = ifelse(type == "synthetic", yes = "CTGAN", no = type))
  }
}

# Load synthetic data from DataSynthesizer (based on optimized parameterization) ----

e = 0
k = 2

df_datasynthesizer <- data.frame()

for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"datasynthesizer/sds_datasynthesizer_",d,"_k_",k,"_e_",e,"_m_",c,"_n_",j,".csv"))
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

      # Splitting the string
      # split_string <- strsplit(as.character(df_compare$Variable), ": S_pMSE")
      # df_compare$Variable <- sapply(split_string, `[`, 1)
      
      df_compare$type <- df_compare$data
      df_compare$data <- d
      df_datasynthesizer <- rbind(df_datasynthesizer,df_compare) %>%
        mutate(type = ifelse(type == "synthetic", yes = "DataSynthesizer", no = type))

  }
}

# Load synthetic data from other data synthesizers ----

df_synthpop <- data.frame()

for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))

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
    
    # Splitting the string
    # split_string <- strsplit(as.character(df_compare$Variable), ": S_pMSE")
    # df_compare$Variable <- sapply(split_string, `[`, 1)
    
    df_compare$type <- df_compare$data
    df_compare$data <- d
    df_synthpop <- rbind(df_synthpop,df_compare) %>%
      mutate(type = ifelse(type == "synthetic", yes = "Synthpop", no = type))
    
  }
}

# Graph ----

df_comparison <- rbind(df_ctgan, df_datasynthesizer, df_synthpop)
names(df_comparison) <- tolower(names(df_comparison))

df_comparison_ods <- filter(df_comparison,type == "observed") %>%
  select(-type)

df_comparison_ods <- unique(df_comparison_ods)
df_comparison_ods$type = "observed"

df_comparison_sds <- filter(df_comparison,type != "observed")
df_comparison <- rbind(df_comparison_ods, df_comparison_sds)

df_graph_data <- df_comparison %>%
  filter(variables %in% c("age","income","height","weight","wkabdur","mmarital","mmarr","msepdiv")) %>%
  # filter(variables %in% c("mmarr","msepdiv")) %>%
  mutate(drop = ifelse((variables == "mmarr" | variables == "msepdiv") & pct == 0, yes = 1, no = 0)) %>%
  filter(drop == 0) %>%
  mutate(value2 = ifelse((variables == "mmarr" | variables == "msepdiv"), yes = round2(as.numeric(as.character(value)),0), no = NA)) %>%
  mutate(value = ifelse(!is.na(value2), yes = as.character(value2), no = as.character(value))) %>%
  select(-drop,-value2)
df_graph_data %>% filter(variables %in% c("mmarr","msepdiv")) %>% print(n=15)

df_graph_data$value <- factor(as.character(df_graph_data$value), levels = str_sort(unique(df_graph_data$value), numeric = TRUE))
df_graph_data$value <- fct_relevel(df_graph_data$value, "NA", after = Inf)

df_graph <- ggplot(df_graph_data, aes(x = value, y = pct, shape = type, color = type, group = type)) +
  geom_point(data = subset(df_graph_data, type!="observed"), position = position_dodge(width = .9), size = 2) +
  geom_bar(data = subset(df_graph_data, type=="observed"), stat = "identity", alpha = .2) +
  facet_wrap( ~ variables, scales = "free") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph
