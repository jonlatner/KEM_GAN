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
library(ggh4x)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

#functions
options(scipen=999) 

# https://github.com/clairelittle/psd2022-comparing-utility-risk/blob/main/code/CIO_Confidence_Interval_Overlap.R
CIO_function = function(orig_glm, syn_glm){
  
  # put them into a form so it is easier to extract the coefficients etc.
  # syn_glm <- list(as.data.frame(summary(syn_glm)$coef))
  # orig_glm <- as.data.frame(summary(orig_glm)$coef)
  
  # orig_tibble = as_tibble(orig_glm) %>% mutate('names' = rownames(orig_glm))
  # syn_tibble = as_tibble(syn_glm[[1]]) %>%  mutate('names' = rownames(syn_glm[[1]]))
  
  # join the original and synth
  combined = left_join(select(orig_tibble, names, Estimate, `Std. Error`), 
                       syn_tibble, by = 'names', suffix = c('_orig', '_syn'))
  
  # now compute std. diff and ci overlap
  results = combined %>% 
    mutate('std.coef_diff' = abs(Estimate_orig - Estimate_syn) / `Std. Error_orig`,
           'orig_lower' = Estimate_orig - 1.96 * `Std. Error_orig`, 
           'orig_upper' = Estimate_orig + 1.96 * `Std. Error_orig`,
           'syn_lower' = Estimate_syn - 1.96 * `Std. Error_syn`, 
           'syn_upper' = Estimate_syn + 1.96 * `Std. Error_syn`) %>%
    mutate('ci_overlap' = 0.5 * (((pmin(orig_upper, syn_upper) - pmax(orig_lower, syn_lower)) / (orig_upper - orig_lower)) + 
                                   ((pmin(orig_upper, syn_upper) - pmax(orig_lower, syn_lower)) / (syn_upper - syn_lower)))) %>%
    select(names, 'std.coef_diff', 'ci_overlap') %>% 
    filter(names != '(Intercept)') %>%
    replace_na(list('std.coef_diff' = 0,'ci_overlap' = 0)) %>% # replace NA with zero
    # set negative overlaps to zero
    mutate('ci_overlap_noNeg' = ifelse(ci_overlap <0, 0, ci_overlap)) %>%
    
    # return the mean/median overall coefficients for each measure
    summarize('mean_std_coef_diff' = mean(std.coef_diff, na.rm=TRUE), 
              'median_std_coef_diff' = median(std.coef_diff, na.rm=TRUE),
              'mean_ci_overlap' = mean(ci_overlap, na.rm=TRUE), 
              'median_ci_overlap' = median(ci_overlap, na.rm=TRUE),
              # add in the overlaps where negatives were changed to zeros
              'mean_ci_overlap_noNeg' = mean(ci_overlap_noNeg, na.rm=TRUE), 
              'median_ci_overlap_noNeg' = median(ci_overlap_noNeg, na.rm=TRUE))
  
  return(results)
}

# dataset ----

data <- c("sd2011_clean_small","sd2011_clean_small_categorical")
df_regression_data <- data.frame()
df_regression_data_cio <- data.frame()
c=5 # multiple copies
parents = c(2)
privacy = c(0)
epochs = c(600)

# Load synthetic data from datasynthesizer ----


for (d in data) {
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
  for (e in privacy) {
    for (k in parents) {
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"datasynthesizer/sds_datasynthesizer_",d,"_k_",k,"_e_",e,"_m_",c,"_n_",j,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        sds$smoke <- as.numeric(sds$smoke == "YES")
        sds_list$syn[[j]] <- sds  # use when m>1
      }
    }
  }
  
  
  sdg_datasynthesizer <- sds_list
  
  
  # Load synthetic data from ctgan ----
  
  
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
  for (e in epochs) {
    for (j in 1:c) {
      sds <- read.csv(paste0(synthetic_data,"ctgan/sds_ctgan_data_",d,"_epochs_",e,"_m_",c,"_n_",j,".csv"))
      sds[sds == ""] <- NA
      sds <- sds %>%
        mutate_if(is.character, as.factor)
      sds$smoke <- as.numeric(sds$smoke == "YES")
      sds_list$syn[[j]] <- sds  # use when m>1
      # sds_list$syn <- sds # use when m==1
    }
  }
  
  
  sdg_ctgan <- sds_list
  
  
  # Load utility from synthpop data ----
  
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  df_ods$smoke <- as.numeric(df_ods$smoke == "YES")
  sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
  for (j in 1:c) {
    sds <- read.csv(paste0(synthetic_data,"synthpop/sds_synthpop_",d,"_m_",c,"_n_",j,".csv"))
    sds[sds == ""] <- NA
    sds <- sds %>%
      mutate_if(is.character, as.factor)
    sds$smoke <- as.numeric(sds$smoke == "YES")
    sds_list$syn[[j]] <- sds  # use when m>1
  }
  
  sdg_synthpop <- sds_list
  
  # Logit regression ----
  
  ## ods
  glm_model_ods <- glm(smoke ~ sex + edu, data = df_ods, family = "binomial")
  output <- as.data.frame(summary(glm_model_ods)$coef)
  orig_tibble = as_tibble(output) %>% 
    mutate('names' = rownames(output)) %>%
    select("names", "Estimate", "Std. Error") %>%
    mutate(type = "observed",
           sdg = "original")
  
  ## sds
  sdg <- c("datasynthesizer","ctgan","synthpop")
  for (s in sdg) {
    sds <- get(paste0("sdg_",s))
    glm_model_sds <- glm.synds(smoke ~ sex + edu, data = sds, family = "binomial")
    
    ## sds
    output <- as.data.frame(summary(glm_model_sds)$coef) 
    syn_tibble = as_tibble(output) %>% mutate('names' = rownames(output)) %>%
      rename("Estimate" = "xpct(Beta)",
             "Std. Error" = "xpct(se.Beta)",
             "z value" = "xpct(z)",
             "Pr(>|xpct(z)|)" = "Pr(>|xpct(z)|)") %>%
      select("names", "Estimate", "Std. Error") %>%
      mutate(type = "synthetic",
             sdg = s)
    
    ## CIO overlap function (Little)
    output_cio <- CIO_function(orig_tibble,syn_tibble)
    df_cio <- data.frame(cio=output_cio$mean_ci_overlap)
    df_cio$dv <- "smoke"
    df_cio$sdg <- s
    df_cio$data <- d
    df_regression_data_cio <- rbind(df_regression_data_cio,df_cio)
    
    regression_data <- rbind(orig_tibble,syn_tibble) %>%
      rename("term"="names", 
             "estimate"="Estimate",
             "std.error"="Std. Error") 
    regression_data$dv <- "smoke"
    regression_data$sdg <- s
    regression_data$data <- d
    df_regression_data <- rbind(df_regression_data,regression_data)
  }
}


# Graph regression ----

df_regression_data_2 <- df_regression_data %>%
  filter(term!="(Intercept)") %>%
  select(term, estimate, std.error, type, dv, sdg, data) %>%
  mutate(type = ifelse(type == "synthetic", yes = sdg, no = type))
df_regression_data_2 <- unique(df_regression_data_2)
df_regression_data_2 <- droplevels(df_regression_data_2)

table(df_regression_data_2$type)

df_regression_data_2$type <- factor(df_regression_data_2$type, 
                                    levels = c("synthpop", "datasynthesizer", "ctgan", "observed"))

df_regression_data_2$data <- factor(df_regression_data_2$data, 
                                levels = c("sd2011_clean_small", "sd2011_clean_small_categorical"),
                                labels = c("SD2011","SD2011 (categorical)"))


df_graph <- ggplot(df_regression_data_2, aes(x = estimate, y = term, color = type)) +
  geom_point(position = position_dodge(width = 0.9)) +
  facet_wrap(~data,nrow = 2) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height = 0, position = position_dodge(width = 0.9)) +
  labs(x = "Estimated Coefficients", y = "Independent Variables") +
  scale_y_discrete(limits = rev(unique(df_regression_data_2$term))) +
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

ggsave(plot = df_graph, paste0(graphs,"graph_utility_compare_dataset_regression.pdf"), height = 8, width = 6)

# Graph cio ----

df_regression_data_cio_graph <- df_regression_data_cio

df_regression_data_cio_graph$data <- factor(df_regression_data_cio_graph$data, 
                                    levels = c("sd2011_clean_small", "sd2011_clean_small_categorical"),
                                    labels = c("SD2011","SD2011 (categorical)"))

df_graph <- ggplot(df_regression_data_cio_graph, aes(x = sdg, y = cio, fill = data)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  labs(x = "Synthetic data generator (SDG)", y = "Confidencce interval overlap (CIO)") +
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

ggsave(plot = df_graph, paste0(graphs,"graph_utility_compare_dataset_regression_cio.pdf"), height = 4, width = 8)
