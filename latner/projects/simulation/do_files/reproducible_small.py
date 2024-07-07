import numpy as np
import matplotlib.pyplot as plt
np.random.seed(1234)

# Original data - customer purchase patterns
customer_data = np.random.normal(loc=10, scale=1, size=1000)

# Differential privacy parameters
privacy_budget = 0.1
scale_parameter = 0.5 # Adjust based on experimentation
# Function to add Laplace noise to data
def add_noise(data, epsilon, scale):
 noise = np.random.laplace(0, scale, size=len(data))
 return data + noise / epsilon
# Differential privacy mechanism
noisy_data = add_noise(customer_data, privacy_budget, scale_parameter)
# Analysis on noisy data
# Perform analysis on 'noisy_data' to extract insights while preserving privacy
print(noisy_data)

# Plotting the original and noisy data
plt.figure(figsize=(10, 6))
plt.plot(customer_data, 'o-', label='Original Data')
#plt.plot(noisy_data, 's-', label='Noisy Data', alpha=0.7)
plt.xlabel('Customer Index')
plt.ylabel('Purchase Amount')
plt.title('Customer Purchase Data with Differential Privacy')
plt.legend(loc='lower center', bbox_to_anchor=(0.5, -0.2), ncol=2)
plt.grid(True)
plt.tight_layout()

# Show the plot
plt.show()

