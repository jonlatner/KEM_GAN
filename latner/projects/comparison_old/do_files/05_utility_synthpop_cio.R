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
synthetic_data = "data_files/synthetic/synthpop/"
graphs = "graphs/synthpop/"
tables = "tables/synthpop/"

setwd(main_dir)

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

# Load original data ----
data <- c("sd2011_clean_small")
copies = c(5) # multiple copies

for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv")) %>%
      mutate(bmi = weight/(height^2)*10000) 
    df_ods$smoke <- as.numeric(df_ods$smoke == "YES")
    df_ods[df_ods < 0] <- NA
    df_ods[df_ods == ""] <- NA
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
    for (j in 1:c) {
      sds_list$syn[[j]] <- sds_list$syn[[j]] %>%
        mutate_if(is.character, as.factor) %>%
        mutate(bmi = weight/(height^2)*10000)
      sds_list$syn[[j]]$smoke <- as.numeric(sds_list$syn[[j]]$smoke == "YES")
    }
  }
}


# LPM regression ----

# model 1
## ods
lpm_model_ods <- lm(smoke ~ sex + age + edu + bmi, data = df_ods)
output <- as.data.frame(summary(lpm_model_ods)$coef)
orig_tibble = as_tibble(output) %>% 
  mutate('names' = rownames(output)) %>%
  select("names", "Estimate", "Std. Error") %>%
  mutate(type = "observed")

## sds
lpm_model_sds <- lm.synds(smoke ~ sex + age + edu + bmi, data = sds_list)
output <- as.data.frame(summary(lpm_model_sds)$coef) 
syn_tibble = as_tibble(output) %>% mutate('names' = rownames(output)) %>%
  rename("Estimate" = "xpct(Beta)",
         "Std. Error" = "xpct(se.Beta)",
         "z value" = "xpct(z)",
         "Pr(>|xpct(z)|)" = "Pr(>|xpct(z)|)") %>%
  select("names", "Estimate", "Std. Error") %>%
  mutate(type = "synthetic")

## CIO overlap function (Little)
output_cio <- CIO_function(orig_tibble,syn_tibble)
df_compare_mean_cio_little <- data.frame(output_cio$mean_ci_overlap)
colnames(df_compare_mean_cio_little) <- "cio"
df_compare_mean_cio_little$model <- "lpm"
df_compare_mean_cio_little$dv <- "smoke"
df_compare_mean_cio_little$type <- "little"
df_compare_mean_cio_little$sdg <- "synthpop"

## CIO overlap function (Synthpop)
model <- lm.synds(smoke ~ sex + age + edu + bmi, data = sds_list, family = "binomial")
df_compare <- compare(model, df_ods)
# df_compare_plot <- df_compare$ci.plot$data
df_compare_mean_cio_synthpop <- data.frame(df_compare$mean.ci.overlap)
colnames(df_compare_mean_cio_synthpop) <- "cio"
df_compare_mean_cio_synthpop$model <- "lpm"
df_compare_mean_cio_synthpop$dv <- "smoke"
df_compare_mean_cio_synthpop$type <- "synthpop"
df_compare_mean_cio_synthpop$sdg <- "synthpop"

df_regression_data_lpm <- rbind(orig_tibble,syn_tibble) %>%
  rename("term"="names", 
         "estimate"="Estimate",
         "std.error"="Std. Error") 

df_regression_data_lpm$model <- "lpm"
df_regression_data_lpm$dv <- "smoke"
df_regression_data_lpm$sdg <- "synthpop"

df_regression_lpm_cio <- rbind(df_compare_mean_cio_little,df_compare_mean_cio_synthpop)


# Logit regression ----

# model 1
## ods
glm_model_ods <- glm(smoke ~ sex + age + edu + bmi, data = df_ods, family = "binomial")
output <- as.data.frame(summary(glm_model_ods)$coef)
orig_tibble = as_tibble(output) %>% 
  mutate('names' = rownames(output)) %>%
  select("names", "Estimate", "Std. Error") %>%
  mutate(type = "observed")

## sds
glm_model_sds <- glm.synds(smoke ~ sex + age + edu + bmi, data = sds_list, family = "binomial")
output <- as.data.frame(summary(glm_model_sds)$coef) 
syn_tibble = as_tibble(output) %>% mutate('names' = rownames(output)) %>%
  rename("Estimate" = "xpct(Beta)",
         "Std. Error" = "xpct(se.Beta)",
         "z value" = "xpct(z)",
         "Pr(>|xpct(z)|)" = "Pr(>|xpct(z)|)") %>%
  select("names", "Estimate", "Std. Error") %>%
  mutate(type = "synthetic")

## CIO overlap function (Little)
output_cio <- CIO_function(orig_tibble,syn_tibble)
df_compare_mean_cio_little <- data.frame(output_cio$mean_ci_overlap)
colnames(df_compare_mean_cio_little) <- "cio"
df_compare_mean_cio_little$model <- "glm"
df_compare_mean_cio_little$dv <- "smoke"
df_compare_mean_cio_little$type <- "little"
df_compare_mean_cio_little$sdg <- "synthpop"

## CIO overlap function (Synthpop)
model <- glm.synds(smoke ~ sex + age + edu + bmi, data = sds_list, family = "binomial")
df_compare <- compare(model, df_ods)
# df_compare_plot <- df_compare$ci.plot$data
df_compare_mean_cio_synthpop <- data.frame(df_compare$mean.ci.overlap)
colnames(df_compare_mean_cio_synthpop) <- "cio"
df_compare_mean_cio_synthpop$model <- "glm"
df_compare_mean_cio_synthpop$dv <- "smoke"
df_compare_mean_cio_synthpop$type <- "synthpop"
df_compare_mean_cio_synthpop$sdg <- "synthpop"

df_regression_data_glm <- rbind(orig_tibble,syn_tibble) %>%
  rename("term"="names", 
         "estimate"="Estimate",
         "std.error"="Std. Error") 

df_regression_data_glm$model <- "glm"
df_regression_data_glm$dv <- "smoke"
df_regression_data_glm$sdg <- "synthpop"

df_regression_glm_cio <- rbind(df_compare_mean_cio_little,df_compare_mean_cio_synthpop)

# Linear regression ----

# model 1
## ods
lm_model_ods <- lm(log(income) ~ sex + age + edu + bmi, data = df_ods)
output <- as.data.frame(summary(lm_model_ods)$coef)
orig_tibble = as_tibble(output) %>% 
  mutate('names' = rownames(output)) %>%
  select("names", "Estimate", "Std. Error") %>%
  mutate(type = "observed")

## sds
lm_model_sds <- lm.synds(log(income) ~ sex + age + edu + bmi, data = sds_list)
output <- as.data.frame(summary(lm_model_sds)$coef) 
syn_tibble = as_tibble(output) %>% mutate('names' = rownames(output)) %>%
  rename("Estimate" = "xpct(Beta)",
         "Std. Error" = "xpct(se.Beta)",
         "z value" = "xpct(z)",
         "Pr(>|xpct(z)|)" = "Pr(>|xpct(z)|)") %>%
  select("names", "Estimate", "Std. Error") %>%
  mutate(type = "synthetic")

## CIO overlap function (Little)
output_cio <- CIO_function(orig_tibble,syn_tibble)
df_compare_mean_cio_little <- data.frame(output_cio$mean_ci_overlap)
colnames(df_compare_mean_cio_little) <- "cio"
df_compare_mean_cio_little$model <- "lm"
df_compare_mean_cio_little$dv <- "LN income"
df_compare_mean_cio_little$type <- "little"
df_compare_mean_cio_little$sdg <- "synthpop"

## CIO overlap function (Synthpop)
model <- lm.synds(log(income) ~ sex + age + edu + bmi, data = sds_list)
df_compare <- compare(model, df_ods)
# df_compare_plot <- df_compare$ci.plot$data
df_compare_mean_cio_synthpop <- data.frame(df_compare$mean.ci.overlap)
colnames(df_compare_mean_cio_synthpop) <- "cio"
df_compare_mean_cio_synthpop$model <- "lm"
df_compare_mean_cio_synthpop$dv <- "LN income"
df_compare_mean_cio_synthpop$type <- "synthpop"
df_compare_mean_cio_synthpop$sdg <- "synthpop"

df_regression_data_lm <- rbind(orig_tibble,syn_tibble) %>%
  rename("term"="names", 
         "estimate"="Estimate",
         "std.error"="Std. Error") 

df_regression_data_lm$model <- "lm"
df_regression_data_lm$dv <- "LN income"
df_regression_data_lm$sdg <- "synthpop"

df_regression_lm_cio <- rbind(df_compare_mean_cio_little,df_compare_mean_cio_synthpop)

# Combine and save ----
df_regression_cio <- rbind(df_regression_glm_cio,df_regression_lm_cio,df_regression_lpm_cio)
df_regression_data <- rbind(df_regression_data_glm,df_regression_data_lm,df_regression_data_lpm)

# Save output 
write.csv(df_regression_data, paste0(tables,"synthpop_utility_regression_plot.csv"), row.names=FALSE)
write.csv(df_regression_cio, paste0(tables,"synthpop_utility_regression_cio.csv"), row.names=FALSE)

df_regression_cio
