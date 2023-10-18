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
# main_dir = "N:/Ablagen/D01700-KEM/Latner/little_etal_2021/"
main_dir = "/Users/jonathanlatner/Documents/GitHub/IAB/simulation_data/continuous_dim/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/"
tables = "tables/"

setwd(main_dir)


# Load data (cols = 10) ----

rows = c("100000") # Rows/observations
cols = c(10, 15, 20) # Columns/variables

for (r in rows) {
  for (c in cols) {
    df_ods <- read.csv(paste0(original_data,"ods_rows_",r,"_cols_",c,".csv"))

    # convert continuous variables to bins
    continuous_vars <- sapply(df_ods, function(x) is.numeric(x))
    continuous_var_names <- names(df_ods[continuous_vars])
    df_ods_binned <- df_ods
    for (col_name in continuous_var_names) {
      bins <- seq(min(df_ods_binned[[col_name]])-1, max(df_ods_binned[[col_name]])+1, length.out = 21) # 21 points to get 20 bins
      labels <- round(bins[-length(bins)],0)  # Remove the last element of breaks
      df_ods_binned[[col_name]] <- cut(df_ods_binned[[col_name]], breaks = bins, labels = labels)
    }
    
    summary(df_ods_binned)
    
    df_ods_long <- df_ods_binned %>%
      mutate(index = row_number()) %>%
      pivot_longer(cols = -index, names_to = "variables", values_to = "value")%>%
      select(-index) %>%
      group_by(variables, value) %>%
      tally() %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      mutate(pct = n/total)
    
    head(df_ods_long)
    
    summary(df_ods_long)
    
    # reorder variable/value names
    df_ods_long$value <- factor(df_ods_long$value, levels=sort(as.numeric(as.character(unique(df_ods_long$value)))))
    df_ods_long$variables <- reorder(df_ods_long$variables, as.numeric(gsub("var_", "", as.character(df_ods_long$variables))))
    
    # graph
    df_graph <- ggplot(df_ods_long, aes(x = value, y = pct)) +
      geom_bar(position = "dodge", stat = "identity") +
      facet_wrap( ~ variables, scales = "free_x", labeller = labeller(.cols = label_both), ncol = 5) +
      xlab("") +
      ylab("") +
      theme_bw() +
      guides(colour = guide_legend(nrow = 1)) +
      theme(panel.grid.minor = element_blank(), 
            legend.position = "bottom",
            legend.key.width=unit(1, "cm"),
            axis.text.x = element_text(angle = 90, hjust = 1),
            axis.line.y = element_line(color="black", linewidth=.5),
            axis.line.x = element_line(color="black", linewidth=.5)
      )
    
    df_graph
    
    ggsave(plot = df_graph, paste0(graphs,"graph_descriptives_rows_100000_cols_",c,".pdf"), height = 6, width = 10)
    
  }
}
