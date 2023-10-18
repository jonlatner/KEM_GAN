'''
Top commands
'''

# load libraries
## Basics 
import os
import pandas as pd
import numpy as np

import seaborn as sns
import matplotlib.pyplot as plt

#main_dir = "N:/Ablagen/D01700-KEM/Latner/simulation/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/simulation_data/continuous/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

os.chdir(main_dir)

'''
LOAD DATA
'''

df_ods = pd.read_csv(os.path.join(original_data,"ods_0.csv"), index_col=False)

"""
Select continuous variables
"""

continuous_vars = df_ods.select_dtypes(include=['int64', 'float64']).columns
df_continuous = df_ods[continuous_vars]

df_continuous['index'] = range(len(df_continuous))
df_continuous = pd.melt(df_continuous, id_vars=['index'], var_name='variable', value_name='value')
df_continuous 

"""
Table
"""

df_continuous_table = df_continuous.groupby('variable')['value'].agg(['min','max', 'mean', 'std', 'median']).reset_index()
df_continuous_table["number"] = np.arange(len(df_continuous_table))+1
df_continuous_table=df_continuous_table[["number","variable",'min','max', 'mean', 'std', 'median']]
print(df_continuous_table)

# create table
df_continuous_table.to_latex(os.path.join(tables,"table_variables_continuous.tex"),index=False, float_format="%.2f")

"""
Table
"""

g = sns.FacetGrid(df_continuous,col="variable",sharey=False,sharex=False)
g.map(sns.kdeplot, "value")
g.set_titles("{col_name}")
g.fig.tight_layout()
g.savefig(os.path.join(graphs, "graph_continuous.pdf"), format="pdf")
plt.show()

"""
Graph decile frequency within continuous variables
"""

df_continuous_decile = df_ods[continuous_vars]

# Cut the continuous variables into deciles
for var in continuous_vars:
    df_continuous_decile[var+'_decile'] = pd.qcut(df_continuous_decile[var], 10, labels=False,duplicates='drop')

# Drop the original continuous variables from the DataFrame
df_continuous_decile = df_continuous_decile.drop(continuous_vars, axis=1)
df_continuous_decile 

# Reshape from wide to long by unique values for each variable within Dataset
df_continuous_decile['index'] = range(len(df_continuous_decile))
df_long = pd.melt(df_continuous_decile, id_vars=['index'], var_name='variable', value_name='value')

# Count values within each variable
df_continuous_decile_long = df_long.groupby(['variable', 'value'], dropna=False).size().reset_index(name='count')

# Preprocess the data to include missing values as a separate category
df_continuous_decile_long['value'] = df_continuous_decile_long['value'].fillna('NA')

# Graph
g = sns.catplot(
    data=df_continuous_decile_long, 
    sharex=False, sharey=False, 
    y='count', x = "value", col="variable",
    kind="bar", legend=False, 
    palette = "deep", 
    col_wrap=3
    )
g.set_axis_labels("", "Count")
g.set_titles("{col_name}")
g.fig.tight_layout()

# Show the plot
g.savefig(os.path.join(graphs, "graph_continuous_deciles.pdf"), format="pdf")
plt.show()

