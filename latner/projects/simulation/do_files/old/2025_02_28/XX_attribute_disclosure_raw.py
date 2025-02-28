print("Hello, World!")
import numpy as np
import pandas as pd

# Generate the original dataset exactly as described
np.random.seed(42)

# Multinomial probabilities for combinations of 4 binary variables
# Ensuring no (1,1,1,1) until the last record
combinations = [(0, 0, 0, 0), (0, 0, 0, 1), (0, 0, 1, 0), (0, 0, 1, 1),
                (0, 1, 0, 0), (0, 1, 0, 1), (0, 1, 1, 0), (0, 1, 1, 1),
                (1, 0, 0, 0), (1, 0, 0, 1), (1, 0, 1, 0), (1, 0, 1, 1),
                (1, 1, 0, 0), (1, 1, 0, 1), (1, 1, 1, 0)]
probabilities = [1/15] * len(combinations)  # Equal probabilities for simplicity

# Generate the original dataset
original_data_sample = np.random.choice(len(combinations), size=999, p=probabilities)
original_data_records = np.array([combinations[i] for i in original_data_sample])
unique_record = np.array([[1, 1, 1, 1]])
original_data = np.vstack([original_data_records, unique_record])

# Generate the synthetic dataset (without the unique record)
synthetic_data_sample = np.random.choice(len(combinations), size=1000, p=probabilities)
synthetic_data_records = np.array([combinations[i] for i in synthetic_data_sample])
synthetic_data = synthetic_data_records

# Calculate DiSCO
matches = np.array([np.array_equal(original_data[i], synthetic_data[i]) for i in range(len(original_data))])
disco_measure = 100 * matches.sum() / len(original_data)

# Display the result
print("DiSCO measure:", disco_measure)
