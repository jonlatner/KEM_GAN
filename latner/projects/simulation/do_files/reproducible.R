# Load necessary library
if(!require("VGAM")) install.packages("VGAM", dependencies=TRUE)
library(VGAM)

# Step 1: Create a dataset
set.seed(123)
data <- rnorm(1000, mean = 50, sd = 10)  # A dataset with 1000 samples from a normal distribution

# Step 2: Define the query (mean of the dataset)
true_mean <- mean(data)

# Step 3: Implement differential privacy
# Custom function to generate Laplace noise
generate_laplace_noise <- function(n, scale) {
  u <- runif(n, min = -0.5, max = 0.5)
  return(scale * sign(u) * log(1 - 2 * abs(u)))
}

# Function to add Laplace noise to a value
add_laplace_noise <- function(value, epsilon, sensitivity) {
  noise <- generate_laplace_noise(1, sensitivity / epsilon)
  return(value + noise)
}

# Parameters for differential privacy
epsilon <- 4.0  # Privacy budget
sensitivity <- 1.0  # Sensitivity of the query

# Query with differential privacy
dp_mean <- add_laplace_noise(true_mean, epsilon, sensitivity)

# Step 4: Compare results
cat("True mean of the dataset:", true_mean, "\n")
cat("Mean with differential privacy:", dp_mean, "\n")

# Visualize the effect of differential privacy
dp_means <- replicate(1000, add_laplace_noise(true_mean, epsilon, sensitivity))
hist(dp_means, breaks=30, main="Distribution of Differentially Private Means", xlab="Differentially Private Mean")
abline(v=true_mean, col="red", lwd=2, lty=2)
legend("topright", legend=c("True Mean"), col="red", lty=2, lwd=2)
