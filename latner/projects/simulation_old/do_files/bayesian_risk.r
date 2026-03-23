# Load packages
library(ggplot2)

# Number of candidates
K <- 16

# Uniform prior
prior <- rep(1/K, K)

# Example likelihoods (toy numbers, sum not required to be 1)
likelihood <- c(0.030, 0.040, 0.050, 0.040,
                0.030, 0.060, 0.220, 0.040,
                0.050, 0.080, 0.060, 0.050,
                0.070, 0.060, 0.030, 0.030)

# Step 1: Compute unnormalized posterior = prior * likelihood
unnormalized <- prior * likelihood

# Step 2: Normalize to sum to 1
posterior <- unnormalized / sum(unnormalized)

# Combine into a table
posterior_table <- data.frame(
  candidate = paste0("c", 1:K),
  prior = round(prior, 4),
  likelihood = round(likelihood, 3),
  posterior = round(posterior, 3)
)

print(posterior_table)

# MAP estimate (most likely candidate)
map_candidate <- posterior_table$candidate[which.max(posterior)]
map_prob <- max(posterior)

cat("\nMAP choice:", map_candidate, "with posterior probability", map_prob, "\n")

# Visualization
ggplot(posterior_table, aes(x = candidate)) +
  geom_bar(aes(y = prior), stat = "identity", fill = "steelblue", alpha = 0.6) +
  geom_bar(aes(y = posterior), stat = "identity", fill = "darkorange", alpha = 0.6) +
  labs(y = "Probability",
       title = "Bayesian Disclosure Risk: Prior vs Posterior",
       subtitle = "Blue = Prior (uniform), Orange = Posterior (updated)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))