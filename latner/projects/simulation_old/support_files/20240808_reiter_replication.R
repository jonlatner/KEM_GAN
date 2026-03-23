# Replication Reiter et al. 2014


## Functions we need

# Draw random values from a Dirichlet distribution
rdirichlet <- function(alpha) {
  s <- sapply(alpha, function(x) {
    s <- rgamma(1, x)
    return(s)
  })
  s <- s / sum(s)
  s
}

# Calculate the logged multinomial coefficient
lmultinomial_coefficient <- function(n, k) {
  (lgamma(n + 1) - sum(lgamma(k + 1)))
}


# My implementation of equation 9
# It expects theta to be a list of the H different draws
calculate_reiter_prob <- function(n, synthetic_data, theta, median = TRUE) {
  res <- t(sapply(theta, function(y)
    exp(
      sapply(y, function(x)
        lmultinomial_coefficient(n, synthetic_data) + sum(synthetic_data * log(x), na.rm = T))
    )))
  
  if (median) {
    res <- apply(res, 2, median)
  } else {
    res <-  apply(res, 2, mean)
  }
  
  return(res)
}

## Set up the original database
categories <- sample(0:14, 999, replace = TRUE)
database <- c(categories, 15)
table(database)



# In this example with a = 0.0001
# And using the mean to summarize the H draws

a <- 1
median <- FALSE
n <- 1000

## Get synthetic data
# Drawing 5 synthetic datasets from each synthesizer
m_synthetic_datasets <- replicate(5, rmultinom(1, n, rdirichlet(table(database) + a)), simplify = F)


## For one synthetic dataset
synthetic_data <- m_synthetic_datasets[[1]]



# Get the h (h = 1000) thetas drawn from the dirichlet distributions for the 16 worlds
h_dirich <- replicate(1000, lapply(lapply(0:15, function(x)
  table(
    factor(c(database[-1000], x), levels = 0:15)
  ) + a), function(y)
    rdirichlet(y)), simplify = F)

# This is equation 9 (if median = F)
prob_Z_c_l <- calculate_reiter_prob(n, synthetic_data, h_dirich, median = median)

# Very small probabilities
prob_Z_c_l

# We have to do that for the m synthetic data sets and take the product
# Equation 7

prob_Z_c <- apply(sapply(m_synthetic_datasets, function(synthetic_data)
  calculate_reiter_prob(n, synthetic_data, h_dirich, median = median)),
  1,
  prod)

# The product makes these even smaller
prob_Z_c

# Now Equation 6
# these are the probabilities reported in table 1
prob_Z_c / sum(prob_Z_c)

# Using the mean is instable. This shows if we set a = 1 and repeat all steps from line 65 a couple of times
# Note this is for the same m synthetic datasets. The only difference is the drawn thetas
# Using the median is stable. Using the exact same synthetic data and same H draws from the synthesizer.

res_mean <- NULL
res_median <- NULL
for(i in 1:10){
  h_dirich <- replicate(1000, lapply(lapply(0:15, function(x)
    table(
      factor(c(database[-1000], x), levels = 0:15)
    ) + a), function(y)
      rdirichlet(y)), simplify = F)
  
  
  prob_Z_c_mean <- apply(sapply(m_synthetic_datasets, function(synthetic_data)
    calculate_reiter_prob(n, synthetic_data, h_dirich, median = FALSE)),
    1,
    prod)
  
  prob_Z_c_median <- apply(sapply(m_synthetic_datasets, function(synthetic_data)
    calculate_reiter_prob(n, synthetic_data, h_dirich, median = TRUE)),
    1,
    prod)
  
  
  # Now Equation 6
  # these are the probabilities reported in table 1
  res_mean <- rbind(res_mean, prob_Z_c_mean / sum(prob_Z_c_mean))
  res_median <- rbind(res_median, prob_Z_c_median / sum(prob_Z_c_median))
  
  cat(i, "\n")
}

res_mean
res_median
