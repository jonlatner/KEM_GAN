'''
Top commands
'''

# load packages
import os
import pandas as pd
import numpy as np
import random
import seaborn as sns
import matplotlib.pyplot as plt


# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/IAB/simulation_data/continuous_dim/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

os.chdir(main_dir)

# Set the random seed
np.random.seed(1234)
random.seed(1234)

# beginning commands
pd.set_option('display.max_columns', None)  # Display all columns
pd.set_option('display.width', 100)  # Auto-wrap columns based on the window width
pd.set_option('display.float_format', '{:.2f}'.format)

'''
Create data
'''

# Dimensions
rows = [50000, 100000, 500000] # Rows/observations
cols = [10] # Columns/variables

df_output = pd.DataFrame()
df_sds = pd.DataFrame()

for r in rows:
    for c in cols:
        # Create an empty dictionary to store the data
        data = {}
        
        # Generate data for each variable
        for i in range(1, c+1):  
            variable_name = f'var_{i}'
            data[variable_name] = np.round(np.random.normal(100, 20, r),0)

        for i in range(6, c+1):  
            variable_name = f'var_{i}'
            data1 = np.round(np.random.normal(100, 10, int(r * 0.3)),0)  # Generate data for the first peak
            data2 = np.round(np.random.normal(50, 15, int(r * 0.7)),0)  # Generate data for the second peak
            data[variable_name] = np.concatenate((data1, data2))
    
        for i in range(8, c+1):
            variable_name = f'var_{i}'
            data1 = np.round(np.random.normal(50, 15, int(r * 0.3)), 0)  # Generate data for the first peak with lower mean and smaller standard deviation
            data2 = np.round(np.random.normal(100, 10, int(r * 0.7)), 0)  # Generate data for the second peak with higher mean and larger standard deviation
            data[variable_name] = np.concatenate((data1, data2))
        
        # Create the DataFrame original data set (ods)
        df_ods = pd.DataFrame(data)
    
        '''
        Save
        '''
    
        # Create a unique filename based on the values
        filename = f"ods_rows_{r}_cols_{c}.csv"
    
        df_ods.to_csv(os.path.join(original_data, filename), index=False)
            
        print("rwos:",r)
        print("cols:",c)

df_ods.describe()

'''
Graph
'''

# Filter only continuous variables (numeric columns)
continuous_vars = df_ods.select_dtypes(include=['float64', 'int64'])

# Melt the DataFrame to create a long-form data structure
df_long = continuous_vars.melt(var_name='Variable', value_name='Value')

# Create histograms using Seaborn facet grid
g = sns.FacetGrid(df_long, col='Variable', col_wrap=5, sharex=True, sharey=True, height=3, aspect=1.2)
g.map(plt.hist, 'Value', bins=20, edgecolor='k')

# Set labels for the facets
g.set_axis_labels('Value', 'Frequency')

# Adjust spacing between subplots
plt.tight_layout()

# Show the plot
plt.show()
