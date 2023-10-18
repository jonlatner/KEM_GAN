'''
Top commands
'''

# load packages
import os
import pandas as pd
import numpy as np
import random

# file paths - adapt main_dir pathway
#main_dir = "N:/Ablagen/D01700-KEM/Latner/simulation/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/simulation_cat/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

os.chdir(main_dir)

# beginning commands
pd.options.display.max_columns = None
pd.options.display.width = 100
pd.set_option('display.float_format', '{:.2f}'.format)

# Set the random seed
np.random.seed(1234)
random.seed(1234)

'''
Create data
'''

# Set the number of observations
n = 1000

# Create an empty dictionary to store the data
data = {}

# Generate data for each variable
for i in range(1, 5):  # 4 dichotomous variables
    variable_name = f'var_{i}'
    data[variable_name] = [random.choice(["A", "B"]) for _ in range(n)]

# Create the DataFrame original data set (ods)
df_ods = pd.DataFrame(data)

'''
Save
'''

df_ods.to_csv(os.path.join(original_data,"ods_0.csv"), index=False)

'''
Do extra stuff
'''

# Print the DataFrame
print(df_ods)

# Concatenate all columns into a single string column
unique_combinations = df_ods.copy()
unique_combinations['combined'] = unique_combinations.astype(str).agg(''.join, axis=1)

# Count the number of unique combinations
unique_combinations = len(unique_combinations['combined'].drop_duplicates())

# Print the result
print("Number of unique combinations:", unique_combinations)

# Count the percentage of each combination of variables
comb_counts = df_ods.groupby(['var_1', 'var_2', 'var_3', 'var_4']).size().reset_index(name='Count')
comb_counts['Percentage'] = comb_counts['Count'] / n * 100

print(comb_counts)

print(df_ods)


