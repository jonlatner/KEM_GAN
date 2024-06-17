'''
TOP COMMANDS
'''

# load libraries
import numpy as np
import pandas as pd
import random
import os
from sklearn.linear_model import LinearRegression

# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"
data_files = "data_files/"
ods_data = "data_files/original/"
sds_data = "data_files/synthetic/"

os.chdir(main_dir)

# beginning commands
pd.options.display.max_columns = None
pd.options.display.width = 100
pd.set_option('display.float_format', '{:.2f}'.format)

# Set the random seed
random.seed(1234)

import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression

# Simulate some data
n = 100
Y1 = np.random.normal(0, 1, n)
Y2 = 2 * Y1 + np.random.normal(0, 1, n)
Y3 = 0.5 * Y1 + 0.5 * Y2 + np.random.normal(0, 1, n)
Y4 = Y1 - Y2 + Y3 + np.random.normal(0, 1, n)

# Create a DataFrame
data = pd.DataFrame({'Y1': Y1, 'Y2': Y2, 'Y3': Y3, 'Y4': Y4})

# Generate synthetic data
synthetic_data = pd.DataFrame(index=data.index, columns=data.columns)
synthetic_data

# Step 1: Generate synthetic Y1
synthetic_data['Y1'] = np.random.choice(data['Y1'], size=n, replace=True)
synthetic_data.head()

# Step 2: Generate synthetic Y2 conditional on Y1
model_Y2 = LinearRegression().fit(data[['Y1']], data['Y2'])
pred_Y2 = model_Y2.predict(data[['Y1']])
residuals_Y2 = data['Y2'] - pred_Y2
synthetic_data['Y2'] = model_Y2.predict(synthetic_data[['Y1']]) + np.random.normal(0, np.std(residuals_Y2), n)

# Step 3: Generate synthetic Y3 conditional on Y1 and Y2
model_Y3 = LinearRegression().fit(data[['Y1', 'Y2']], data['Y3'])
pred_Y3 = model_Y3.predict(data[['Y1', 'Y2']])
residuals_Y3 = data['Y3'] - pred_Y3
synthetic_data['Y3'] = model_Y3.predict(synthetic_data[['Y1', 'Y2']]) + np.random.normal(0, np.std(residuals_Y3), n)

# Step 4: Generate synthetic Y4 conditional on Y1, Y2, and Y3
model_Y4 = LinearRegression().fit(data[['Y1', 'Y2', 'Y3']], data['Y4'])
pred_Y4 = model_Y4.predict(data[['Y1', 'Y2', 'Y3']])
residuals_Y4 = data['Y4'] - pred_Y4
synthetic_data['Y4'] = model_Y4.predict(synthetic_data[['Y1', 'Y2', 'Y3']]) + np.random.normal(0, np.std(residuals_Y4), n)

# Plot the distributions of original and synthetic data
variables = ['Y1', 'Y2', 'Y3', 'Y4']

plt.figure(figsize=(12, 8))

for i, var in enumerate(variables, 1):
    plt.subplot(2, 2, i)
    sns.kdeplot(data[var], label='Original', shade=True)
    sns.kdeplot(synthetic_data[var], label='Synthetic', shade=True)
    plt.title(f'Distribution of {var}')
    plt.legend()

plt.tight_layout()
plt.show()


print(synthetic_data.head())
print(data.head())
