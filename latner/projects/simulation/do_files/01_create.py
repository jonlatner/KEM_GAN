'''
TOP COMMANDS
'''

import numpy as np
import pandas as pd
import seaborn as sns
import random
import os
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression

# Set the random seed
random.seed(1234)

# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"
data_files = "data_files/"
ods_data = "data_files/original/"
sds_data = "data_files/synthetic/"
graph_folder = "graphs/"

os.chdir(main_dir)

# beginning commands

pd.options.display.max_columns = None
pd.options.display.width = 100
pd.set_option('display.float_format', '{:.2f}'.format)

# Simulate some data

np.random.seed(1234)
n = 100
Y1 = np.random.normal(0, 1, n)
Y2 = 2 * Y1 + np.random.normal(0, 1, n)
Y3 = 0.5 * Y1 + 0.5 * Y2 + np.random.normal(0, 1, n)
Y4 = Y1 - Y2 + Y3 + np.random.normal(0, 1, n)

Y1 = np.random.normal(0, 1, n)
Y2 = np.random.normal(0, 1, n)
Y3 = np.random.normal(0, 1, n)
Y4 = np.random.normal(0, 1, n)

# Create a DataFrame

data = pd.DataFrame({'Y1': Y1, 'Y2': Y2, 'Y3': Y3, 'Y4': Y4})

# Plot the correlations

corr = data.corr()
# Set the figure size
plt.figure(figsize=(10, 8))

# Create the heatmap
sns.heatmap(corr, annot=True, cmap='coolwarm', vmin=-1, vmax=1)

# Add title
plt.title('Correlation Heatmap')

# Define the full path for the saved file
file_path = os.path.join(graph_folder, 'ods_data.png')

# Save the plot
plt.savefig(file_path)

# Show the plot
plt.show()

# Generate synthetic data

synthetic_data = pd.DataFrame(index=data.index, columns=data.columns)

# Step 1: Generate synthetic Y1

synthetic_data['Y1'] = np.random.choice(data['Y1'], size=n, replace=True)
synthetic_data

# Loop to generate synthetic Y2, Y3, ..., Yp

for i in range(1, data.shape[1]):
    
    predictors = data.columns[:i]
    target = data.columns[i]
    
    model = LinearRegression().fit(data[predictors], data[target])
    pred = model.predict(data[predictors])
    residuals = data[target] - pred
    
    synthetic_data[target] = model.predict(synthetic_data[predictors]) + np.random.normal(0, np.std(residuals), n)

synthetic_data

# Plot the correlations synthetic data

corr = synthetic_data.corr()
# Set the figure size
plt.figure(figsize=(10, 8))

# Create the heatmap
sns.heatmap(corr, annot=True, cmap='coolwarm', vmin=-1, vmax=1)

# Add title
plt.title('Correlation Heatmap')

# Define the full path for the saved file
file_path = os.path.join(graph_folder, 'sds_data.png')

# Save the plot
plt.savefig(file_path)

# Show the plot
plt.show()

# Plot the distributions of original and synthetic data

variables = data.columns

plt.figure(figsize=(12, 8))

for i, var in enumerate(variables, 1):
    plt.subplot(2, 2, i)
    sns.kdeplot(data[var], label='Original', fill=True)
    sns.kdeplot(synthetic_data[var], label='Synthetic', fill=True)
    plt.title(f'Distribution of {var}')
    plt.legend()

plt.tight_layout()

# Define the full path for the saved file
file_path = os.path.join(graph_folder, 'correlation.png')

# Save the plot
plt.savefig(file_path)

plt.show()
