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

# Load data ----

ods <- SD2011
ods[ods < 0] <- NA
ods[ods == ""] <- NA
ods <- ods %>%
  mutate_if(is.character, as.factor)

df_ods <- ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods$wkabdur <- as.numeric(df_ods$wkabdur)


# Synthesize data

s1 <- syn(df_ods, m = 3)

### Logit model

glm_model <- glm.synds(smoke ~ edu + age + sex + bmi, data = s1, family = "binomial")

compare(glm_model, df_ods,plot = "coef")
# compare(f1, df_ods, print.coef = TRUE, plot = "coef")

### linear model

lm_model <- lm.synds(log(income) ~ edu + age + sex + bmi, data = s1, family = "binomial")

# f1 <- glm.synds(smoke ~ sex + age + edu + , data = s1, family = "binomial")
compare(lm_model, df_ods,plot = "coef")

