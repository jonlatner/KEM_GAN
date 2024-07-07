# top commands ----
# https://desfontain.es/blog/differential-privacy-in-practice.html

rm(list=ls(all=TRUE))
set.seed(123)

# library
library(tidyverse)
library(LaplacesDemon)

# create data ----

df_green_y <- data.frame(
  green = rep(1, 1000)
)

df_green_n <- data.frame(
  green = rep(0, 1001)
)

# generate original data
df_ods <- rbind(df_green_y,df_green_n)
rm(df_green_y,df_green_n)

# graph data ----

df_graph <- df_ods %>%
  group_by(green) %>%
  tally() %>%
  ungroup()

ggplot(df_graph, aes(x = green, y = n)) +
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.5) +
  theme_minimal()

# add noise ----

# Define sensitivity and epsilon
sensitivity <- 1  # Sensitivity for counting queries is 1
epsilon <- 0.5    # You can adjust epsilon according to your privacy needs

# Calculate the scale parameter for the Laplace distribution
b <- sensitivity / epsilon

# Generate Laplace noise for both counts
laplace_noise_green_n <- round(rlaplace(1, location = 0, s = b),0)
laplace_noise_green_y <- round(rlaplace(1, location = 0, s = b),0)

laplace_noise_green_n
laplace_noise_green_y

# Add the noise to the counts

df_green_y <- data.frame(
  green = rep(1, 1000 + laplace_noise_green_y)
)

df_green_n <- data.frame(
  green = rep(0, 1001 + laplace_noise_green_n)
)

# generate synthetic data
df_sds <- rbind(df_green_y,df_green_n)
rm(df_green_y,df_green_n)

# compare ----

df_ods$source <- "observed"
df_sds$source <- "synthetic"

df_compare <- rbind(df_ods,df_sds)

df_graph <- df_compare %>%
  group_by(green,source) %>%
  tally() %>%
  ungroup()

ggplot(df_graph, aes(x = green, y = n, fill = source)) +
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.5) +
  theme_minimal()
