# top commands ----
# https://desfontain.es/blog/differential-privacy-in-practice.html

rm(list=ls(all=TRUE))
set.seed(123)

# Load necessary library
library(tidyverse)

# Define parameters
mean_value <- 1000
scale <- 1  # You can adjust the scale (b) as needed

# Define the Laplace distribution function
dlaplace <- function(x, mean, scale) {
  return(1/(2*scale) * exp(-abs(x - mean)/scale))
}

# Generate data for plotting
x_values <- seq(mean_value - 10, mean_value + 10, by = 0.01)
y1_values <- dlaplace(x_values, mean_value, scale) # green eyes
y2_values <- dlaplace(x_values, mean_value+1, scale) # blue eyes

# Create data frame for ggplot
data <- data.frame(x = x_values, 
                   y1 = y1_values,
                   y2 = y2_values
)

# Plot the Laplace distribution
ggplot(data) +
  geom_line(aes(x = x, y = y1, color = "y1")) +
  geom_line(aes(x = x, y = y2, color = "y2")) +
  scale_color_manual(values = c("y1" = "green", "y2" = "blue"), 
                     labels = c("Green", "Blue")) +  # Define colors and labels
  scale_x_continuous(limits = c(995, 1006), 
                     breaks = seq(996, 1006, 2)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(2, "cm"),
        )

