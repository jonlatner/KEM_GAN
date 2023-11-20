'''
TOP COMMANDS
'''

# load libraries
## Basics 
import os
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/IAB/simulation_data/continuous_dim/"

data_files = "data_files/"
original_data = "data_files/original/"

os.chdir(main_dir)

# beginning commands
pd.set_option('display.width', 1000)
pd.set_option('display.float_format', '{:.2f}'.format)
pd.set_option('display.max_columns', None)

os.chdir(main_dir)

'''
Load data
'''

# Dimensions
rows = [50000] # Rows/observations
cols = [10] # Columns/variables

for r in rows:
    for c in cols:
        
        # load data
        filename_ods = f"ods_rows_{r}_cols_{c}.csv"
        df_ods = pd.read_csv(os.path.join(original_data, filename_ods), index_col=False)

df_ods.describe()

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
