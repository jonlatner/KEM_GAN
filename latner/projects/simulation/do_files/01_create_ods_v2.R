# Load necessary library
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Define the number of samples
n <- 1000

# Define the possible combinations of y
combinations <- expand.grid(y1 = c(0, 1), y2 = c(0, 1), y3 = c(0, 1), y4 = c(0, 1))

# Define C-16 and c16
C_minus_16 <- combinations[1:15, ]
c16 <- combinations[16, ]

# Initialize an empty matrix to store the results
y <- matrix(0, nrow = n, ncol = 4)

# Generate the first n-1 samples from the multinomial distribution
for (i in 1:(n-1)) {
  # Sample an index from 1 to 15 with equal probability
  sampled_index <- sample(1:15, 1)
  y[i, ] <- as.numeric(C_minus_16[sampled_index, ])
}

# Set the last sample to c16
y[n, ] <- as.numeric(c16)

# Convert the matrix to a data frame
y_df <- as.data.frame(y)

# Display the first few rows of the dataset
head(y_df)

# Display the last row to verify y1000 is c16
tail(y_df, 1)
