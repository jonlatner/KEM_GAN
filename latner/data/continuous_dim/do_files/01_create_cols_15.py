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
import scipy.stats as stats


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
rows = [50000, 100000, 200000] # Rows/observations

df_output = pd.DataFrame()
df_sds = pd.DataFrame()

for r in rows:
    # Create an empty dictionary to store the data
    data = {}
        
    # Normal distribution variables with mean approximately 100
    normal_vars = {f"normal_var{i}": np.round(np.random.normal(100, 10, r)) for i in range(1, 6)}
        
    # Left-skewed distribution variables with mean approximately 100
    left_skewed_vars = {f"left_skewed_var{i}": np.round(stats.skewnorm.rvs(-10, loc=105, scale=20, size=r)) for i in range(1, 6)}
        
    # Right-skewed distribution variables with mean approximately 100
    right_skewed_vars = {f"right_skewed_var{i}": np.round(stats.skewnorm.rvs(10, loc=95, scale=20, size=r)) for i in range(1, 6)}
        
    # Combine all variables into a single dictionary
    all_vars = {**normal_vars, **left_skewed_vars, **right_skewed_vars}
        
    # Create the DataFrame original data set (ods)
    df_ods = pd.DataFrame(all_vars)
    
    '''
    Save
    '''
    
    # Create a unique filename based on the values
    filename = f"ods_rows_{r}_cols_15.csv"
    
    df_ods.to_csv(os.path.join(original_data, filename), index=False)
            
    print("rows:",r)
 
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
