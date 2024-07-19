# load libraries
library(tidyverse)
library(synthpop)

set.seed(123456)

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

rm(D,c_16,C_minus_16,combinations)

ods <- df_ods

# Create a frequency table
ods$combine <- paste(ods$var1, ods$var2, ods$var3, ods$var4, sep = "")

ods <- ods %>%
  select(-matches("var"))
frequency <- as.data.frame(table(ods))

sds <- syn(df_ods, m=1, minnumlevels=5)
sds <- sds$syn

formals(syn.cart)

?syn.cart

# Create a frequency table
sds$combine <- paste(sds$var1, sds$var2, sds$var3, sds$var4, sep = "")
sds <- sds %>%
  select(-matches("var"))
frequency <- as.data.frame(table(sds))

# Create a complete data frame with all desired levels and fill missing values with 0
# Desired levels for the x-axis
desired_levels <- c("0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111")
frequency <- frequency %>%
  mutate(combine = as.character(combine)) %>%
  complete(combine = desired_levels, fill = list(Freq = 0))

frequency

ggplot(frequency, aes(x = combine, y = Freq)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = Freq), vjust=-.5)+ 
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )
