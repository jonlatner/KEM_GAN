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

data <- c("sd2011","sd2011_clean","sd2011_clean_small")
parents = c(0, 1, 2)
privacy = c(0)
copies = c(5)

data <- c("sd2011")
parents = c(2)

df_fidelity_plot <- data.frame()
df_comparison <- data.frame()

for (c in copies) {
  for (d in data) {
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
    df_ods <- read.csv(paste0(original_data,d,".csv")) # load original data
    # df_ods$bmi <- round(df_ods$bmi,0)
    
    for (e in privacy) {
      for (k in parents) {
        for (j in 1:c) {
          sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_",d,"_k_",k,"_e_",e,"_m_",c,"_n_",j,".csv"))
          sds[sds == ""] <- NA
          sds <- sds %>%
            mutate_if(is.character, as.factor)
          # sds$bmi <- round(sds$bmi,0)
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

        utility <- utility.tables(sds_list, df_ods, tables = "twoway")
        utility_plot <- data.frame(utility$utility.plot$data)
        utility_plot$copies <- c
        utility_plot$privacy <- e
        utility_plot$parents <- k
        utility_plot$data <- d
        df_fidelity_plot <- rbind(df_fidelity_plot,utility_plot)
        
        df_comparison <- rbind(df_comparison,df_compare)
      }
    }
  }
}

with(sds,table(nofriend))

with(df_ods,table(nofriend))

with(sds,table(wkabdur))

with(df_ods,table(wkabdur))

summary(df_ods$bmi)
summary(sds$bmi)

with(df_ods,table(round(bmi,0),sex))
with(sds,table(round(bmi,0),sex))

# Graph (frequency) ----

df_comparison$value <- factor(as.character(df_comparison$value), levels = str_sort(unique(df_comparison$value), numeric = TRUE))
df_comparison$value <- fct_relevel(df_comparison$value, "NA", after = Inf)
table(df_comparison$dataset)

df_data <- df_comparison %>%
  filter(variables == "wkabdur")  %>%
  filter(parents == 2)
  
df_data <- droplevels(df_data) 

df_graph <- ggplot(df_data, aes(x = value, y = pct, fill = data, color = data, group = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_nested(parents ~ variables + dataset, labeller = labeller(.rows = label_both)) +
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
