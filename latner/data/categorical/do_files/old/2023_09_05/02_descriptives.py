'''
Top commands
'''

# load libraries
## Basics 
import os
import pandas as pd

import seaborn as sns
import matplotlib.pyplot as plt

#main_dir = "N:/Ablagen/D01700-KEM/Latner/simulation/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/drechsler_latner_2023/simulation_data/categorical/"

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
Graph the frequency of values within categorical variables
"""

num_threshold = 10

# Select categorical variables
categorical_columns = df_ods.select_dtypes(include=['object', 'category']).columns
df_categorical = df_ods[categorical_columns]

# Drop the columns that are categorical with unique values greater than num_threshold
# Find columns
cols_to_drop = [col for col in categorical_columns if df_categorical[col].nunique() > num_threshold]

# Drop columns
df_categorical = df_categorical.drop(cols_to_drop, axis=1)
df_categorical 

# Reshape from wide to long by unique values for each variable within Dataset
# Create a temporary index column
df_categorical['index'] = range(len(df_categorical))
df_long = pd.melt(df_categorical, id_vars=['index'], var_name='Variable', value_name='Value')

# Count values within each variable
df_categorical_long = df_long.groupby(['Variable', 'Value'], dropna=False).size().reset_index(name='Count')

# Preprocess the data to include missing values as a separate category
df_categorical_long['Value'] = df_categorical_long['Value'].fillna('NA')

# Graph
sns.set_theme(style="whitegrid",font_scale=1.5)
g = sns.catplot(
    data=df_categorical_long, 
    y='Count', x = "Value", col="Variable",
    kind="bar", legend=False, 
    palette = "deep", 
    col_wrap=4
    )
g.set_axis_labels("", "Count")
g.set_titles("{col_name}")
g.fig.tight_layout()

g.savefig(os.path.join(graphs, "graph_categorical.pdf"), format="pdf")
plt.show()

