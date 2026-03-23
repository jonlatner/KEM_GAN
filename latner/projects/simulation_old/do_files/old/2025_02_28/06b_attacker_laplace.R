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

#functions
options(scipen=999) 

# Set seed for reproducibility
my.seed = 1237
set.seed(my.seed)

# Create simulated, original data with vulnerable observation ----

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
df_original <- as.data.frame(D)
head(df_original)


df_ods_frequency <- df_original
df_ods_frequency$combine <- paste(df_ods_frequency$var1, df_ods_frequency$var2, df_ods_frequency$var3, df_ods_frequency$var4, sep = "")
df_ods_frequency <- df_ods_frequency %>%
  select(-matches("var"))
df_ods_frequency <- as.data.frame(table(df_ods_frequency))

# Plot
p <- ggplot(df_ods_frequency, aes(x = combine, y=Freq)) +
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

# Loop through attack ----

# Define the 16 possible combinations of four binary variables
combinations <- expand.grid(y1 = c(0, 1), y2 = c(0, 1), y3 = c(0, 1), y4 = c(0, 1))

df_frequency <- data.frame()
for (c in 1:10) {
  for (r in 1:16) {

    # create seed
    my.seed = my.seed + 1
    
    # Load original data 
    df_ods <- df_original
    
    # Drop the last row
    df_ods <- head(df_ods, -1)
    
    # Set the last observation to last_record
    last_record <- combinations[r,]
    print(last_record)
    df_ods[1000,] <- last_record

    # Generate frequency table ----
    
    df_ods_frequency <- df_ods
    df_ods_frequency$combine <- paste(df_ods_frequency$var1, df_ods_frequency$var2, df_ods_frequency$var3, df_ods_frequency$var4, sep = "")
    df_ods_frequency <- df_ods_frequency %>%
      select(-matches("var"))
    
    df_frequency_ods <- as.data.frame(table(df_ods_frequency)) %>%
      rename(value=Freq) 
    
    # Generate laplace noise ---- 
    
    epsilon <- c(.1)
    df_frequency_sds <- data.frame()
    
    for (e in epsilon) {
      ods <- df_frequency_ods %>%
        mutate(epsilon = e)
      
      # Parameters for the Laplace distribution
      location <- 0  # Mean (location parameter)
      sensitivity <- 1 # this is always 1 in integer counting queries (if integers were 2,4,6,etc., then sensitivity would be 2)
      b <- sensitivity/e # scale parameter
      
      # Generate Laplace noise for each value in n
      laplace_noise <- rlaplace(nrow(df_frequency_ods), location, b)
      
      # Add the noise to the n column
      sds <- df_frequency_ods
      sds$Freq <- sds$value + laplace_noise
      sds$value <- NULL
      sds <- sds %>%
        mutate(type = "synthetic",
               n = c,
               last_record = paste(last_record$y1, last_record$y2, last_record$y3, last_record$y4, sep = ""),
               )
      df_frequency_sds <- rbind(df_frequency_sds,sds)
      
      df_ods_frequency <- as.data.frame(table(df_ods_frequency)) %>%
        mutate(type = "original",
               n = c,
               last_record = paste(last_record$y1, last_record$y2, last_record$y3, last_record$y4, sep = ""))
    }
    
    # Combine
    df_frequency <- rbind(df_frequency,df_frequency_sds,df_ods_frequency)
  }
}

# Graph ----


df_graph_sds <- df_frequency %>%
  filter(type == "synthetic") 

df_graph_ods <- df_frequency %>%
  filter(type == "original") 

df_graph_ods <- unique(df_graph_ods)

df_graph <- 
  ggplot() +
  geom_bar(data = df_graph_ods, aes(x = combine, y = Freq, fill = type), position = position_dodge(width=0.9), stat = "identity") +
  geom_boxplot(position = position_dodge(width=0.9), aes(x = combine, y = Freq, fill = type), data = df_graph_sds) +
  facet_wrap(~last_record, labeller = "label_both") +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,25)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        axis.title.x = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph
