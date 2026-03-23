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
library(synthpop)
library(tidyverse)
library(ggh4x) # facet_nested

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
graphs = "graphs/"

setwd(main_dir)

#functions
options(scipen=999) 

# Set seed for reproducibility
my.seed = 1237
set.seed(my.seed)

# Create simulated data ----

# Number of observations
n <- 1000

# Define the 16 possible combinations of four binary variables
combinations <- expand.grid(y1 = c(0, 1), y2 = c(0, 1), y3 = c(0, 1), y4 = c(0, 1))

# Define c_16 and C_−16
c_16 <- combinations[16,]
C_minus_16 <- combinations[-16,]

# Initialize the dataset
D <- data.frame(matrix(ncol = 4, nrow = n))
colnames(D) <- c("var1", "var2", "var3", "var4")

# Sample the first 999 observations from C_minus_16 with equal probability
for (i in 1:(n-1)) {
  sampled_row <- sample(1:15, 1)
  D[i,] <- C_minus_16[sampled_row,]
}

# Set the 1000th observation to c_16
D[1000,] <- c_16

# Convert to data frame and print the first few rows
df_ods <- as.data.frame(D)
head(df_ods)

# Save ----
write.csv(df_ods, paste0(original_data,"simulated.csv"), row.names = FALSE)

# Generate a data frame with 1,000 observations and 4 columns, each column having random "0"/"1" values
simulated_data <- data.frame(
  var1 = sample(c(0, 1), size = 1000, replace = TRUE),
  var2 = sample(c(0, 1), size = 1000, replace = TRUE),
  var3 = sample(c(0, 1), size = 1000, replace = TRUE),
  var4 = sample(c(0, 1), size = 1000, replace = TRUE),
  type = "original"
)


# Reshape the data for plotting
df_combine_long <- simulated_data %>%
  pivot_longer(!type, names_to = "variable", values_to = "value") %>%
  mutate(value = as.factor(value)) %>%
  group_by(type,variable,value) %>%
  tally() %>%
  group_by(type,variable) %>%
  mutate(total = sum(n),
         pct = n/total) %>%
  ungroup()

df_combine_long

# Plot using facet_wrap
p <- ggplot(subset(df_combine_long,type == "original"), aes(x = value, y=n)) +
  geom_bar(stat = 'identity',position = position_dodge(.9)) +
  geom_text(aes(label = n),vjust = -0.5, size = 4,position = position_dodge(.9)) +
  facet_wrap(~variable) +
  theme_bw() +
  scale_y_continuous(limits = c(0,700), breaks = seq(0,700,100)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

p
ggsave(p, filename = paste0(graphs,"graph_variable_frequency.pdf"), height = 4, width = 6, units = "in")


df_ods_frequency <- simulated_data
df_ods_frequency$combine <- paste(df_ods_frequency$var1, df_ods_frequency$var2, df_ods_frequency$var3, df_ods_frequency$var4, sep = "")
df_ods_frequency <- df_ods_frequency %>%
  select(-matches("var"))
df_ods_frequency <- as.data.frame(table(df_ods_frequency))
df_ods_frequency$pct <- (df_ods_frequency$Freq / nrow(df_ods_frequency)) * 100

# Plot using facet_wrap
p <- ggplot(subset(df_ods_frequency,type == "original"), aes(x = combine, y=Freq)) +
  geom_bar(stat = 'identity',position = position_dodge(.9)) +
  geom_text(aes(label = Freq),vjust = -0.5, size = 4,position = position_dodge(.9)) +
  theme_bw() +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

p

ggsave(p, filename = paste0(graphs,"graph_variable_histogram.pdf"), height = 4, width = 6, units = "in")
