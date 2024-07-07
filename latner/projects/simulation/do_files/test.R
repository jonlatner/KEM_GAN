rm(list=ls(all=TRUE))

library(tidyverse)
library(VGAM)
set.seed(1234)

# Generate data for plotting ---- 
# Define parameters
mean_value <- 10000
scale <- 1  # You can adjust the scale (b) as needed

x_values <- seq(mean_value - 10, mean_value + 10, by = 0.01)
y_values <- dlaplace(x_values, mean_value, scale)

# Create data frame for ggplot
df_ods <- data.frame(x = x_values,
                     y = y_values,
                     n = mean_value,
                     type = " observed"
                     )

# Plot the data
df_data <- data.frame()
df_data <- rbind(df_ods)
ggplot(df_ods, aes(x = x, y = y, color = type)) +
  geom_line() +
  theme_bw() +
  geom_text(aes(label = ifelse(y == max(y), n, "")), vjust = -.5, hjust = 0.5,show.legend=F) +  # Add text labels
  theme(axis.title = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank(),         
        legend.key.width = unit(2, "cm"),
        )

# Generate Laplace noise ---- 

epsilon <- c(0.01, 0.1)
for (e in epsilon) {
  # Add Laplace noise to the mean value
  laplace_noise <- rlaplace(1000, location = 0, scale = scale/e)
  noisy_value <- round(mean_value + mean(laplace_noise))
  x_values <- seq(noisy_value - 10, noisy_value + 10, by = 0.01)
  y_values <- dlaplace(x_values, noisy_value, scale) 
  
  # Create data frame for ggplot
  df_sds <- data.frame(x = x_values, 
                       y = y_values,
                       n = noisy_value,
                       type = paste0("e = ", e)
  )
  df_data <- rbind(df_data,df_sds)
  
}

# Plot the data
ggplot(df_data, aes(x = x, y = y, color = type)) +
  geom_line() +
  theme_bw() +
  geom_text(aes(label = ifelse(y == max(y), n, "")), vjust = -.5, hjust = 0.5,show.legend=F) +  # Add text labels
  theme(axis.title = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank(),         
        legend.key.width = unit(2, "cm"),
  )
