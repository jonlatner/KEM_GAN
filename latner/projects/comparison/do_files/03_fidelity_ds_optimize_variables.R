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
library(xtable)
library(synthpop)
library(ggh4x)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/datasynthesizer/"
tables = "tables/datasynthesizer/"
graphs = "graphs/datasynthesizer/"

#functions
options(scipen=999) 

# Load utility from datasynthesizer data ----

parents = c(0,1,2,3)
privacy = c(0)
data <- c("sd2011","sd2011_clean","sd2011_clean_small","sd2011_clean_small_categorical")
data <- c("sd2011_clean_small")

# 1 copy
# multiple copies
c=5

df_comparison <- data.frame()

for (d in data) {
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
  for (e in privacy) {
    for (k in parents) {
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_",d,"_k_",k,"_e_",e,"_m_",c,"_n_",j,".csv"))
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        sds_list$syn[[j]] <- sds  # use when m>1
        # sds_list$syn <- sds # use when m==1
        
      }
      
      df_compare <- compare(sds_list,df_ods,utility.stats = "all")
      
      specks <- df_compare$tab.utility[,4]
      output <- data.frame(specks)
      output$variables <- rownames(output)
      rownames(output) <- NULL
      output$data = d
      output$copies = c
      output$parents = k
      output$privacy = e
      
      df_comparison <- rbind(df_comparison,output)
      
    }
  }
}

write.csv(df_comparison, paste0(tables,"datasynthesizer_fidelity_optimize_variables.csv"), row.names=FALSE)

# Graph ----

df_comparison <- read.csv(paste0(tables,"datasynthesizer_fidelity_optimize_variables.csv"))
df_comparison$variables <- factor(as.character(df_comparison$variables))
df_comparison$parents <- factor(as.character(df_comparison$parents))
df_comparison <- df_comparison %>%
  filter(data == "sd2011_clean_small")

df_graph <- ggplot(df_comparison, aes(x = variables, y = specks, fill = parents)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  # facet_wrap( ~ data, labeller = labeller(.rows = label_both), scales = "free_y", nrow = 4) +
  # ylab("Kolmogorov-Smirnov (lower is better)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        # legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_fidelity_optimize_variables.pdf"), height = 4, width = 6)

# df_utility <- utility.tables(sds_list, df_ods, tables = "twoway",plot.stat = "")
# df_utility$utility.plot
# ggsave(plot = last_plot(), paste0(graphs,"graph_datasynthesizer_sd2011.pdf"), height = 4, width = 8)
# 
# f1 <- lm.synds(bmi ~ sex + age + edu + height + weight, data = sds_list)
# compare(f1, df_ods)
# compare(f1, df_ods, print.coef = TRUE, plot = "coef")
# 
# f2 <- lm.synds(log(income) ~ sex + age + edu, data = sds_list)
# compare(f2, df_ods)
# compare(f2, df_ods, print.coef = TRUE, plot = "coef")
# ggsave(plot = last_plot(), paste0(graphs,"graph_datasynthesizer_sd2011_bmi_cio.pdf"), height = 4, width = 8)
