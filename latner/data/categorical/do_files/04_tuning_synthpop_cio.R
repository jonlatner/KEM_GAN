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
library(broom)
library(tidyverse)
library(ggh4x) # facet_nested
library(synthpop)
library(fastDummies) # dummy_cols
library(mice)

# FOLDERS - ADAPT THIS PATHWAY
# main_dir = "N:/Ablagen/D01700-KEM/Latner/little_etal_2021/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/drechsler_latner_2023/simulation_data/categorical/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
graphs = "graphs/synthpop/"
tables = "tables/synthpop/"

#functions
options(scipen=999) 

CIO_function = function(orig_tibble, syn_tibble){
  
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
    # select(names, 'std.coef_diff', 'ci_overlap') %>%
    filter(names != '(Intercept)') %>%
    replace_na(list('std.coef_diff' = 0,'ci_overlap' = 0)) %>% # replace NA with zero
    # set negative overlaps to zero
    mutate('ci_overlap_noNeg' = ifelse(ci_overlap <0, 0, ci_overlap)) %>%
    
    # return the mean/median overall coefficients for each measure
    mutate('mean_std_coef_diff' = mean(std.coef_diff, na.rm=TRUE),
           'median_std_coef_diff' = median(std.coef_diff, na.rm=TRUE),
           'mean_ci_overlap' = mean(ci_overlap, na.rm=TRUE),
           'median_ci_overlap' = median(ci_overlap, na.rm=TRUE),
           # add in the overlaps where negatives were changed to zeros
           'mean_ci_overlap_noNeg' = mean(ci_overlap_noNeg, na.rm=TRUE),
           'median_ci_overlap_noNeg' = median(ci_overlap_noNeg, na.rm=TRUE))
  
  return(results)
}

# Create fake synthetic data ----

# df_ods <- read.csv(paste0(original_data,"ods_0.csv"))
# df_ods <- df_ods %>%
#   mutate_if(is.character, as.factor)
# copies <- c(1, 5, 10)
# for (c in copies) {
#   df_synds <- syn(df_ods, m = c)
#   saveRDS(df_synds, paste0(data_files,"synthetic/synds_",c,".rds"))
# }

# Load data ----

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))
df_ods[df_ods == ""] <- NA

df_ods <- df_ods %>%
  mutate_if(is.character, as.factor) 

# Load synthetic data ----

df_synds_1 <- readRDS(paste0(data_files,"synthetic/synds_1.rds"))
df_synds_5 <- readRDS(paste0(data_files,"synthetic/synds_5.rds"))
df_synds_10 <- readRDS(paste0(data_files,"synthetic/synds_10.rds"))

df_results_sds <- data.frame()
df_results_ods <- data.frame()
df_output_cio <- data.frame()

dep_var <- colnames(df_ods)

cp <- c(0.00000001, 0.000001, 0.0001, 0.01)
minbucket <- c(5, 10, 25, 50)
copies = c(5, 10)
for (c in copies) {
  sds_list <- get(paste0("df_synds_",c))
  for (dv in dep_var) { 
    for (a in cp) {
      for (b in minbucket) {
        for (j in 1:c) {
          sds <- read.csv(paste0(synthetic_data,"sds_synthpop_tuning_cp_",a,"_b_",b,"_m_",c,"_n_",j,".csv"))
          sds[sds == ""] <- NA
          
          ## for synthetic data, transform dummy variables into categorical variables based on their maximum value
          df <- sds
          # Replace "" with NA
          # df[[dv]] <- ifelse(df[[dv]] == "", NA, df[[dv]])
          # Get the frequency table of the column, after omitting missing values
          freq_table <- table(na.omit(df[[dv]]))
          # Get the value with the highest frequency
          max_freq_value <- names(freq_table)[which.max(freq_table)]
          # Create a new dichotomous variable based on the highest frequency value
          new_dv <- ifelse(df[[dv]] == max_freq_value, "Y", "N")
          # Reinsert missing values from old variable into new variable
          # new_dv <- ifelse(df[[dv]] == "", NA, new_dv)
          # Drop original variable
          df[[dv]] <- NULL
          # Assign the new dichotomous variable to the data frame
          df[[dv]] <- new_dv
          sds<-df
          
          # replace .synds data frame with user created synthetic data, not necessarily from synthpop
          sds <- sds %>%
            mutate_if(is.character, as.factor)
          sds_list$syn[[j]] <- sds
        }
        
        ## for original data, transform dummy variables into categorical variables based on their maximum value
        df <- df_ods %>%
          mutate_if(is.factor,as.character)
        
        # df[[dv]] <- ifelse(df[[dv]] == "", NA, df[[dv]])
        freq_table <- table(na.omit(df[[dv]]))
        max_freq_value <- names(freq_table)[which.max(freq_table)]
        new_dv <- ifelse(df[[dv]] == max_freq_value, "Y", "N")
        # new_dv <- ifelse(df[[dv]] == "", NA, new_dv)
        df[[dv]] <- NULL
        df[[dv]] <- new_dv
        original <- df %>%
          mutate_if(is.character, as.factor)
        
        # model 1
        ## ods
        glm_model <- glm(as.formula(paste0(dv, " ~ .")), data = original, family = "binomial")
        output <- as.data.frame(summary(glm_model)$coef)
        ods_output = as_tibble(output) %>% 
          mutate('names' = rownames(output)) %>%
          select("names", "Estimate", "Std. Error")
        
        results_ods <- ods_output
        results_ods$dep_var <- dv
        results_ods$data <- "ods"
        results_ods$minbucket <- b
        results_ods$cp <- a
        results_ods$copies <- c
        df_results_ods <- rbind(df_results_ods, results_ods)
        
        ## sds
        glm_model <- glm.synds(as.formula(paste0(dv, " ~ .")), data = sds_list, family = "binomial")
        output <- as.data.frame(summary(glm_model)$coef) 
        sds_output = as_tibble(output) %>% mutate('names' = rownames(output)) %>%
          rename("Estimate" = "xpct(Beta)",
                 "Std. Error" = "xpct(se.Beta)",
                 "z value" = "xpct(z)",
                 "Pr(>|xpct(z)|)" = "Pr(>|xpct(z)|)") %>%
          select("names", "Estimate", "Std. Error")
        
        results_sds <- sds_output
        results_sds$dep_var <- dv
        results_sds$data <- "sds"
        results_sds$minbucket <- b
        results_sds$cp <- a
        results_sds$copies <- c
        df_results_sds <- rbind(df_results_sds, results_sds)
        
        ## function
        output_cio <- CIO_function(ods_output,sds_output)
        output_cio$dep_var <- dv
        output_cio$minbucket <- b
        output_cio$cp <- a
        output_cio$copies <- c
        
        df_output_cio <- rbind(df_output_cio,output_cio)
      }
    }
  }
}

df_results_1 <- rbind(df_results_ods,df_results_sds)
df_output_cio_1 <- df_output_cio

df_results_sds <- data.frame()
df_results_ods <- data.frame()
df_output_cio <- data.frame()
copies = c(1)
for (c in copies) {
  sds_list <- get(paste0("df_synds_",c))
  for (dv in dep_var) { 
    for (a in cp) {
      for (b in minbucket) {
        sds <- read.csv(paste0(synthetic_data,"sds_synthpop_tuning_cp_",a,"_b_",b,"_m_",c,"_n_1.csv"))
        sds[sds == ""] <- NA
        
        ## for synthetic data, transform dummy variables into categorical variables based on their maximum value
        df <- sds
        # Replace "" with NA
        # df[[dv]] <- ifelse(df[[dv]] == "", NA, df[[dv]])
        # Get the frequency table of the column, after omitting missing values
        freq_table <- table(na.omit(df[[dv]]))
        # Get the value with the highest frequency
        max_freq_value <- names(freq_table)[which.max(freq_table)]
        # Create a new dichotomous variable based on the highest frequency value
        new_dv <- ifelse(df[[dv]] == max_freq_value, "Y", "N")
        # Reinsert missing values from old variable into new variable
        # new_dv <- ifelse(df[[dv]] == "", NA, new_dv)
        # Drop original variable
        df[[dv]] <- NULL
        # Assign the new dichotomous variable to the data frame
        df[[dv]] <- new_dv
        sds<-df
        
        # replace .synds data frame with user created synthetic data, not necessarily from synthpop
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        sds_list$syn <- sds
        
        ## for original data, transform dummy variables into categorical variables based on their maximum value
        df <- df_ods %>%
          mutate_if(is.factor,as.character)
        
        # df[[dv]] <- ifelse(df[[dv]] == "", NA, df[[dv]])
        freq_table <- table(na.omit(df[[dv]]))
        max_freq_value <- names(freq_table)[which.max(freq_table)]
        new_dv <- ifelse(df[[dv]] == max_freq_value, "Y", "N")
        # new_dv <- ifelse(df[[dv]] == "", NA, new_dv)
        df[[dv]] <- NULL
        df[[dv]] <- new_dv
        original <- df %>%
          mutate_if(is.character, as.factor)
        
        # model 1
        ## ods
        glm_model <- glm(as.formula(paste0(dv, " ~ .")), data = original, family = "binomial")
        output <- as.data.frame(summary(glm_model)$coef)
        ods_output = as_tibble(output) %>% 
          mutate('names' = rownames(output)) %>%
          select("names", "Estimate", "Std. Error")
        
        results_ods <- ods_output
        results_ods$dep_var <- dv
        results_ods$data <- "ods"
        results_ods$minbucket <- b
        results_ods$cp <- a
        results_ods$copies <- c
        df_results_ods <- rbind(df_results_ods, results_ods)
        
        ## sds
        glm_model <- glm.synds(as.formula(paste0(dv, " ~ .")), data = sds_list, family = "binomial")
        output <- as.data.frame(summary(glm_model)$coef) 
        sds_output = as_tibble(output) %>% mutate('names' = rownames(output)) %>%
          rename("Estimate" = "xpct(Beta)",
                 "Std. Error" = "xpct(se.Beta)",
                 "z value" = "xpct(z)",
                 "Pr(>|xpct(z)|)" = "Pr(>|xpct(z)|)") %>%
          select("names", "Estimate", "Std. Error")
        
        results_sds <- sds_output
        results_sds$dep_var <- dv
        results_sds$data <- "sds"
        results_sds$minbucket <- b
        results_sds$cp <- a
        results_sds$copies <- c
        df_results_sds <- rbind(df_results_sds, results_sds)
        
        ## function
        output_cio <- CIO_function(ods_output,sds_output)
        output_cio$dep_var <- dv
        output_cio$minbucket <- b
        output_cio$cp <- a
        output_cio$copies <- c
        
        df_output_cio <- rbind(df_output_cio,output_cio)
      }
    }
  }
}

df_output_cio_2 <- df_output_cio
df_output_cio <- rbind(df_output_cio_1,df_output_cio_2)

df_results_2 <- rbind(df_results_ods,df_results_sds)
df_results <- rbind(df_results_1,df_results_2)

## Summary table 
write.csv(df_results, paste0(tables,"utility_cio_estimates.csv"), row.names=FALSE)

## Clean table 


data_for_clustering <- df_output_cio %>%
  group_by(minbucket, cp, copies) %>%
  summarise(std_diff = mean(mean_std_coef_diff),
            ci_overlap = mean(mean_ci_overlap_noNeg)) %>%
  ungroup() 

write.csv(data_for_clustering, paste0(tables,"utility_cio.csv"), row.names=FALSE)

head(data_for_clustering)
head(df_results)

# Prepare for graphing ----

data_for_clustering <- data_for_clustering %>%
  mutate(minbucket = factor(minbucket),
         cp = as.factor(cp),
         copies = as.factor(copies),
  )  %>%
  arrange(std_diff) %>%
  mutate(n = row_number())

head(data_for_clustering, 10)


## Graph

data_for_clustering_long <- data_for_clustering %>%
  pivot_longer(cols = !c(minbucket, cp, copies,n), names_to = "names", values_to = "value")

df_graph <- ggplot(data_for_clustering_long, aes(y = value, x = n, color = names)) +
  geom_point() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_synthpop_cio_raw.pdf"), height = 4, width = 6)

## Linear model

lm_model_1 <- lm(ci_overlap ~ minbucket + cp + copies, data = data_for_clustering)
lm_model_2 <- lm(std_diff ~ minbucket + cp + copies, data = data_for_clustering)

tidy_model_data_1 <- tidy(lm_model_1) %>% filter(term!="(Intercept)") %>% mutate(dep_var = "CI overlap")
tidy_model_data_2 <- tidy(lm_model_2) %>% filter(term!="(Intercept)") %>% mutate(dep_var = "Std diff")

tidy_model_data <- rbind(tidy_model_data_1, tidy_model_data_2)

## Graph

df_graph <- ggplot(tidy_model_data, aes(y = term, x = estimate, 
                            xmin = estimate - (1.96 * std.error), xmax = estimate + (1.96 * std.error))) +
  geom_point() +
  geom_errorbar() +
  facet_wrap(~dep_var) +
  labs(y = "Variables", x = "Estimate coefficeint with 95% CI") +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "solid", color = "red") +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_synthpop_cio.pdf"), height = 4, width = 6)

## Linear model by copies

# Create an empty list to store the results
tidy_model_data_list <- list()

# Define the values of 'copies' you want to analyze
copies_values <- c(1, 5, 10)
dv <- c("ci_overlap","std_diff")

# Loop through each value of 'copies' and fit the linear model
for (d in dv) {
  for (c in copies_values) {
  lm_model <- lm(paste0(d, " ~ minbucket + cp"), data = subset(data_for_clustering, copies == c))
  tidy_model_data <- tidy(lm_model) %>% filter(term != "(Intercept)") %>% mutate(copies = c, dv = d)
  tidy_model_data_list <- rbind(tidy_model_data_list,tidy_model_data)
  }
}

## Graph with facet wrap

df_graph <- ggplot(tidy_model_data_list, aes(y = term, x = estimate, 
                                 xmin = estimate - (1.96 * std.error), xmax = estimate + (1.96 * std.error))) +
  facet_grid(copies~dv,labeller = labeller(.cols = label_value, .rows = label_both), scales = "free") +
  geom_point() +
  geom_errorbar() +
  labs(y = "Variables", x = "Estimate coefficeint with 95% CI") +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "solid", color = "red") +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_synthpop_cio_facet.pdf"), height = 9, width = 6)

