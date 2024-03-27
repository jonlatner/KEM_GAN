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
library(ggpubr)
library(tidyverse)
library(synthpop)
library(ggh4x) # facet_nested

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

setwd(main_dir)

# Load data ----

df_synthpop_regression_data <- read.csv(paste0(tables,"synthpop/synthpop_utility_regression_plot.csv"))
df_synthpop_regression_cio <- read.csv(paste0(tables,"synthpop/synthpop_utility_regression_cio.csv"))

df_ctgan_regression_data <- read.csv(paste0(tables,"ctgan/ctgan_utility_regression_plot.csv"))
df_ctgan_regression_cio <- read.csv(paste0(tables,"ctgan/ctgan_utility_regression_cio.csv"))

df_datasynthesizer_regression_data <- read.csv(paste0(tables,"datasynthesizer/datasynthesizer_utility_regression_plot.csv"))
df_datasynthesizer_regression_cio <- read.csv(paste0(tables,"datasynthesizer/datasynthesizer_utility_regression_cio.csv"))

df_syndiffix_regression_data <- read.csv(paste0(tables,"syndiffix/syndiffix_utility_regression_plot.csv"))
df_syndiffix_regression_cio <- read.csv(paste0(tables,"syndiffix/syndiffix_utility_regression_cio.csv"))

df_regression_data <- rbind(df_synthpop_regression_data,df_ctgan_regression_data,df_datasynthesizer_regression_data,df_syndiffix_regression_data)
df_regression_data_cio <- rbind(df_synthpop_regression_cio,df_ctgan_regression_cio,df_datasynthesizer_regression_cio,df_syndiffix_regression_cio)

table(df_regression_data$sdg)

# Graph regression ----

df_regression_data <- df_regression_data %>%
  filter(term != "(Intercept)") %>%
  mutate(columns = "Dependent variable",
         rows = "Synthetic data generator (SDG)") %>%
  mutate(term = ifelse(term == "sexMALE", yes = "male",
                       ifelse(term == "eduPRIMARY/NO EDUCATION", yes = "primary.no.education",
                              ifelse(term == "eduSECONDARY", yes = "secondary",
                                     ifelse(term == "eduVOCATIONAL/GRAMMAR", yes = "vocational.grammar", no = term)))))



df_graph <- ggplot(df_regression_data, aes(x = estimate, y = term, color = type)) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height = 0, position = position_dodge(width = 0.9)) +
  labs(x = "Estimated Coefficients", y = "Independent Variables") +
  facet_nested(rows + sdg~columns+dv) +
  scale_y_discrete(limits = rev(unique(df_regression_data$term))) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "solid", color = "red") +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_utility_regression.pdf"), height = 4, width = 8)


# Graph regression ----

df_regression_data_2 <- df_regression_data %>%
  select(term, estimate, std.error, type, model, dv, sdg) %>%
  mutate(type = ifelse(type == "synthetic", yes = sdg, no = type))
df_regression_data_2 <- unique(df_regression_data_2)
df_regression_data_2

table(df_regression_data_2$type)

df_regression_data_2$type <- factor(df_regression_data_2$type, 
                          levels = c("synthpop", "datasynthesizer", "ctgan", "observed"))


df_graph <- ggplot(df_regression_data_2, aes(x = estimate, y = term, color = type)) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height = 0, position = position_dodge(width = 0.9)) +
  labs(x = "Estimated Coefficients", y = "Independent Variables") +
  facet_wrap(~ dv) +
  scale_y_discrete(limits = rev(unique(df_regression_data$term))) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "solid", color = "red") +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_utility_regression.pdf"), height = 4, width = 8)

df_regression_data_cio

# Graph cio ----

df_regression_data_cio_2 <- df_regression_data_cio %>%
  filter(type == "little")%>%
  mutate(columns = "Dependent variable") 


df_graph <- ggplot(df_regression_data_cio_2, aes(x = sdg, y = cio)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  labs(x = "Synthetic data generator (SDG)", y = "Confidencce interval overlap (CIO)") +
  facet_nested_wrap(~columns+dv) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "solid", color = "red") +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_utility_regression_cio.pdf"), height = 4, width = 8)


# Graph cio ----

df_regression_data_2 <- df_regression_data %>%
  select(term, estimate, std.error, type, model, dv, sdg) %>%
  mutate(type = ifelse(type == "synthetic", yes = sdg, no = type))
df_regression_data_2 <- unique(df_regression_data_2)
df_regression_data_2

table(df_regression_data_2$type)

df_regression_data_2$type <- factor(df_regression_data_2$type, 
                                    levels = c("synthpop", "syndiffix", "datasynthesizer", "ctgan", "observed"))


df_graph <- ggplot(df_regression_data_2, aes(x = estimate, y = term, color = type)) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height = 0, position = position_dodge(width = 0.9)) +
  labs(x = "Estimated Coefficients", y = "Independent Variables") +
  facet_wrap(~ dv) +
  scale_y_discrete(limits = rev(unique(df_regression_data$term))) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "solid", color = "red") +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph


df_regression_data_cio_2 <- df_regression_data_cio %>%
  # facet_nested_wrap(~columns+dv) +
  filter(type == "little")%>%
  mutate(columns = "Dependent variable") %>%
  select(sdg,cio, dv) %>%
  pivot_wider(names_from = dv, values_from = cio) %>%
  select(sdg, "LN income", smoke)

df_text <- ggtexttable(df_regression_data_cio_2, rows = NULL)
# Arrange the plots on the same page
df_graph <- ggarrange(df_graph, df_text, 
          ncol = 1, nrow = 3,
          heights = c(1, 0.5, 0.3))

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_utility_regression_cio_both.pdf"), height = 4, width = 8)




