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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/datasynthesizer/"

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

data <- c("ods_vars_4_vals_2")

# Load synthetic data from DataSynthesizer (based on optimized parameterization) ----

e = 0
k = 2
copies = 5

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

      df_compare$sdg <- df_compare$data
      df_compare$data <- d
      df_datasynthesizer <- rbind(df_datasynthesizer,df_compare) %>%
        mutate(sdg = ifelse(sdg == "synthetic", yes = "DataSynthesizer", no = sdg))

  }
}

# Load synthetic data from Synthpop ----

df_synthpop <- data.frame()

for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    df_ods <- df_ods %>%
      mutate_if(is.character, as.factor)
    
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
    for (j in 1:c) {
      sds <- read.csv(paste0(synthetic_data,"synthpop/sds_synthpop_",d,"_m_",c,"_n_",j,".csv"))
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
    
    df_compare$sdg <- df_compare$data
    df_compare$data <- d
    df_synthpop <- rbind(df_synthpop,df_compare) %>%
      mutate(sdg = ifelse(sdg == "synthetic", yes = "Synthpop", no = sdg))
    
  }
}


# Graph ----

df_comparison <- rbind(df_datasynthesizer,df_synthpop)
names(df_comparison) <- tolower(names(df_comparison))

df_comparison_ods <- filter(df_comparison,sdg == "observed") %>%
  select(-sdg)

df_comparison_ods <- unique(df_comparison_ods)
df_comparison_ods$sdg = "observed"

df_comparison_sds <- filter(df_comparison,sdg != "observed")
df_comparison <- rbind(df_comparison_ods, df_comparison_sds)

df_graph_data <- df_comparison 

df_graph <- ggplot(df_graph_data, aes(x = value, y = pct, shape = sdg, fill = sdg, group = sdg)) +
  geom_bar(data = subset(df_graph_data), stat = "identity", position = position_dodge(width = .9)) +
  facet_wrap( ~ variables, scales = "free") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph
