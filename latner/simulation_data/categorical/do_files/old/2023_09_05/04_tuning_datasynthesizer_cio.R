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
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/simulation_cat/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/datasynthesizer/"
tables = "tables/datasynthesizer/"

#functions
source("/Users/jonathanlatner/Google Drive/My Drive/IAB/little_etal_2021/do_files/R/cio_function_multiple.R")
source("/Users/jonathanlatner/Google Drive/My Drive/IAB/little_etal_2021/do_files/R/cio_function.R")
options(scipen=999) 

# Load data ----

# original data

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))

# Get the names of the categorical variables
vars_categorical <- names(df_ods)[sapply(df_ods, function(x) is.character(x) || is.factor(x))]
# Get the names of the continuous variables
vars_continuous <- names(df_ods)[sapply(df_ods, function(x) !is.character(x) && !is.factor(x))]

df <- df_ods
# Loop through each column in the data frame
for (col in names(df)) {
  # Check if the column is categorical (factor or character)
  if (is.character(df[[col]]) || is.factor(df[[col]])) {
    # Replace "" with NA
    df[[col]] <- ifelse(df[[col]] == "", NA, df[[col]])
    # Get the frequency table of the column, after omitting missing values
    freq_table <- table(na.omit(df[[col]]))
    # Get the value with the highest frequency
    max_freq_value <- names(freq_table)[which.max(freq_table)]
    # Create a new dichotomous variable based on the highest frequency value
    new_col <- ifelse(df[[col]] == max_freq_value, 1, 0)
    # Reinsert missing values
    new_col <- ifelse(df[[col]] == "", NA, new_col)
    # Drop original variable
    df[[col]] <- NULL
    # Assign the new dichotomous variable to the data frame
    df[[paste(col,max_freq_value,sep = "_")]] <- new_col
  }
}
df_ods <- df
head(df_ods, 10)
rm(df)

# synthetic data
df_results_sds <- data.frame()
df_results_ods <- data.frame()
df_output_cio <- data.frame()

parents = c(0, 1, 2)
privacy = c(0, 0.1, 1, 5, 10, 20, 30)
copies = c(5, 10)
for (k in parents) {
  for (e in privacy) {
    for (m in copies) {
      # Loop through each categorical variable as dependent variable
      for (dep_var in names(df_ods)) {
        df_models_sds <- list()
        for (j in 1:m) {
          sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_tuning_e_",e,"_k_",k,"_m_",m,"_n_",j,".csv"))
          sds <- sds %>% 
            mutate_if(is.character, factor)
          
          # Transform character and factor variables into dummy variables
          sds_dummies <- dummy_cols(sds, select_columns = vars_categorical, remove_selected_columns = TRUE)
          
          # Select only columns that are in df_ods
          sds_dummies <- sds_dummies[, names(df_ods)]
          
          # Subset the data frame to include only the current dependent variable
          ind_vars <- c(names(df_ods))
          subset_ods <- df_ods[, c(dep_var, setdiff(ind_vars, dep_var))]
          subset_sds <- sds_dummies[, c(dep_var, setdiff(ind_vars, dep_var))]
          
          # remove missing values
          subset_ods<-subset_ods[complete.cases(subset_ods),]
          subset_sds<-subset_sds[complete.cases(subset_sds),]
          
          # Linear or LPM model
          model_sds <- lm(paste(dep_var, "~ ."), data = subset_sds)
          # model_sds <- glm(paste(dep_var, "~ ."), data = subset_sds, family = binomial())
          df_models_sds[[j]] <- model_sds
        }
        
        results_sds <- summary(pool.syn(df_models_sds, rule = "reiter2003"))
        results_sds$dep_var <- dep_var
        results_sds$data <- "sds"
        results_sds$parents <- k
        results_sds$privacy <- e
        results_sds$copies <- m
        results_sds$df<-NULL
        df_results_sds <- rbind(df_results_sds, results_sds)
        
        model_ods <- lm(paste(dep_var, "~ ."), data = subset_ods)
        results_ods <- tidy(model_ods)
        results_ods$dep_var <- dep_var
        results_ods$data <- "ods"
        results_ods$parents <- k
        results_ods$privacy <- e
        results_ods$copies <- m
        df_results_ods <- rbind(df_results_ods, results_ods)
        
        df_models_sds_tibble <- summary(pool.syn(df_models_sds, rule = "reiter2003")) %>%
          rename("Estimate"="estimate",
                 "Std. Error"="std.error",
                 "t value"="statistic",
                 "Pr(>|t|)"="p.value",
                 'names' = 'term') %>%
          as_tibble()
        
        output <- CIO_function_multiple(model_ods,df_models_sds_tibble)
        output$dep_var <- dep_var
        output$parents <- k
        output$privacy <- e
        output$copies <- m
        df_output_cio <- rbind(df_output_cio,output)
      }
    }
  }
}

df_results_1 <- rbind(df_results_ods,df_results_sds)

df_output_cio_1 <- df_output_cio

df_results_sds <- data.frame()
df_results_ods <- data.frame()
df_output_cio <- data.frame()
parents = c(0, 1, 2)
privacy = c(0, 0.1, 1, 5, 10, 20, 30)
copies = c(1)
for (k in parents) {
  for (e in privacy) {
    for (m in copies) {
      # Loop through each categorical variable as dependent variable
      for (dep_var in names(df_ods)) {
        df_models_sds <- list()
        sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_tuning_e_",e,"_k_",k,"_m_",m,"_n_1.csv"))
        sds <- sds %>% 
          mutate_if(is.character, factor)
        
        # Transform character and factor variables into dummy variables
        sds_dummies <- dummy_cols(sds, select_columns = vars_categorical, remove_selected_columns = TRUE)
        
        # Select only columns that are in df_ods
        sds_dummies <- sds_dummies[, names(df_ods)]
        
        # Subset the data frame to include only the current dependent variable
        ind_vars <- c(names(df_ods))
        subset_ods <- df_ods[, c(dep_var, setdiff(ind_vars, dep_var))]
        subset_sds <- sds_dummies[, c(dep_var, setdiff(ind_vars, dep_var))]
        
        # remove missing values
        subset_ods<-subset_ods[complete.cases(subset_ods),]
        subset_sds<-subset_sds[complete.cases(subset_sds),]
        
        # Linear or LPM model
        model_sds <- lm(paste(dep_var, "~ ."), data = subset_sds)
        # model_sds <- glm(paste(dep_var, "~ ."), data = subset_sds, family = binomial())
        
        results_sds <- tidy(model_sds)
        results_sds$dep_var <- dep_var
        results_sds$data <- "sds"
        results_sds$parents <- k
        results_sds$privacy <- e
        results_sds$copies <- m
        results_sds$df<-NULL
        df_results_sds <- rbind(df_results_sds, results_sds)
        
        model_ods <- lm(paste(dep_var, "~ ."), data = subset_ods)
        results_ods <- tidy(model_ods)
        results_ods$dep_var <- dep_var
        results_ods$data <- "ods"
        results_ods$parents <- k
        results_ods$privacy <- e
        results_ods$copies <- m
        df_results_ods <- rbind(df_results_ods, results_ods)
        
        output <- CIO_function(model_ods,model_sds)
        output$dep_var <- dep_var
        output$parents <- k
        output$privacy <- e
        output$copies <- m
        df_output_cio <- rbind(df_output_cio,output)
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

head(df_output_cio,10)

data_for_clustering <- df_output_cio %>%
  group_by(parents, privacy, copies) %>%
  summarise(std_diff = mean(mean_std_coef_diff),
            ci_overlap = mean(mean_ci_overlap_noNeg)) %>%
  ungroup() %>%
  mutate(parents = factor(parents),
         privacy = as.factor(privacy),
         copies = as.factor(copies),
  )  %>%
  arrange(std_diff) %>%
  mutate(n = row_number())

head(data_for_clustering, 10)


## Graph

data_for_clustering_long <- data_for_clustering %>%
  pivot_longer(cols = !c(parents, privacy, copies,n), names_to = "names", values_to = "value")

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

ggsave(plot = df_graph, paste0(graphs,"graph_datasynthesizer_cio_raw.pdf"), height = 4, width = 6)

## Linear model

lm_model_1 <- lm(ci_overlap ~ parents + privacy + copies, data = data_for_clustering)
lm_model_2 <- lm(std_diff ~ parents + privacy + copies, data = data_for_clustering)

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

ggsave(plot = df_graph, paste0(graphs,"graph_datasynthesizer_cio.pdf"), height = 4, width = 6)

## Linear model by copies

# write.csv(df_univ, paste0(tables,"utility_roc_univar.csv"), row.names=FALSE)

# Create an empty list to store the results
tidy_model_data_list <- list()

# Define the values of 'copies' you want to analyze
copies_values <- c(1, 5, 10)
dv <- c("ci_overlap","std_diff")

# Loop through each value of 'copies' and fit the linear model
for (d in dv) {
  for (c in copies_values) {
  lm_model <- lm(paste0(d, " ~ parents + privacy"), data = subset(data_for_clustering, copies == c))
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

ggsave(plot = df_graph, paste0(graphs,"graph_datasynthesizer_cio_facet.pdf"), height = 9, width = 6)

## Summary table 
data_for_clustering$n<-NULL
write.csv(data_for_clustering, paste0(tables,"utility_cio.csv"), row.names=FALSE)

##. checks

parents = c(900)
privacy = c(10)
batch = c(1000)
frequency = c("False")
copies = c(5)
for (k in parents) {
  for (e in privacy) {
    for (m in copies) {
      df_models_sds_check <- list()
      for (j in 1:m) {
        sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_tuning_e_",e,"_k_",k,"_m_",m,"_n_",j,".csv"))
        print(j)
        sds <- sds %>%
          mutate_if(is.character, factor)
        sds_dummies <- dummy_cols(sds, select_columns = vars_categorical, remove_selected_columns = TRUE)
        
        # Select only columns that are in df_ods
        sds_dummies <- sds_dummies[, names(df_ods)]
        
        # Subset the data frame to include only the current dependent variable
        ind_vars <- c(names(df_ods))
        subset_ods <- df_ods[, c(dep_var, setdiff(ind_vars, dep_var))]
        subset_sds <- sds_dummies[, c(dep_var, setdiff(ind_vars, dep_var))]
        
        # remove missing values
        subset_ods<-subset_ods[complete.cases(subset_ods),]
        subset_sds<-subset_sds[complete.cases(subset_sds),]
        
        # Linear or LPM model
        model_sds <- lm(var_1_B ~ ., data = subset_sds)
        
        df_models_sds_check[[j]] <- model_sds
      }
    }
  }
}

results_tibble_check <- summary(pool.syn(df_models_sds_check, rule = "reiter2003")) %>%
  rename("Estimate"="estimate",
         "Std. Error"="std.error",
         "t value"="statistic",
         "Pr(>|t|)"="p.value",
         'names' = 'term') %>%
  select(-df) %>%
  as_tibble()
model_ods_check <- lm(var_1_B ~ ., data = subset_ods)

output <- CIO_function_multiple(model_ods_check,results_tibble_check)
output %>% select(mean_std_coef_diff,mean_ci_overlap_noNeg)
# df_output_cio %>% filter(parents==900, privacy==10, batch==1000, frequency == "False", copies == 5 & dep_var == "var_1_B") %>% select(mean_std_coef_diff,mean_ci_overlap_noNeg)

