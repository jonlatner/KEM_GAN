'''
TOP COMMANDS
'''

# load libraries
## Basics 
import os
import pandas as pd

## Graphing
import seaborn as sns
import matplotlib.pyplot as plt

# file paths - adapt main_dir pathway
#main_dir = "N:/Ablagen/D01700-KEM/Latner/simulation/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/simulation_cat/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/ctgan/"
tables = "tables/ctgan/"

os.chdir(main_dir)

# beginning commands
pd.set_option('display.width', 1000)
pd.set_option('display.float_format', '{:.2f}'.format)

'''
LOAD DATA
'''

# synthetic data
epochs = [30, 60]
discriminator = [1, 5]
batch = [500, 1000]
frequency = [True, False]
copies = [1, 5]

df_combine_sds = []

count = 0
for d in discriminator:
    for e in epochs:
        for b in batch:
            for f in frequency:
                df_multiple_sds = []
                for m in copies: 
                    df_sds = []
                    for j in range(m):    
                        count = count + 1
                        print(count)

                        j = j + 1

                        # Open a unique filename based on the values
                        filename = f"sds_ctgan_tuning_e_{e}_d_{d}_b_{b}_f_{f}_m_{m}_n_{j}.csv"
                        sds = pd.read_csv(os.path.join(synthetic_data, filename))

                        # combine multiple data frames
                        df_sds.append(sds)
                        
                    # Append the DataFrame to the list
                    df_multiple_sds = pd.concat(df_sds, ignore_index=True)
                    # Create new variable for each data set
                    df_multiple_sds["epochs"] =  e
                    df_multiple_sds["discriminator"] = d
                    df_multiple_sds["batch"] = b
                    df_multiple_sds["frequency"] = f
                    df_multiple_sds["copies"] = m

                    df_combine_sds.append(df_multiple_sds)

df_combine_sds = pd.concat(df_combine_sds, ignore_index=True)

# clean the environment
del(b, batch, e, epochs, f, frequency, m, copies, j, filename, d, discriminator, sds, df_multiple_sds)

"""
Graph frequency values within categorical variables
"""

# Reshape from wide to long by unique values for each variable within Dataset
group_vars_1 = ["epochs", "discriminator", "batch", "frequency", "copies"]
melted_df = pd.melt(df_combine_sds, id_vars=group_vars_1, var_name='Variable', value_name='Value')
melted_df 

# Count values within each variable
group_vars_2 = group_vars_1 + ["Variable", "Value"]
count_df = melted_df.groupby(group_vars_2, dropna=False).size().reset_index(name='Count')

# Graph
g = sns.FacetGrid(data=count_df, col="Variable", row = "copies", 
                  # so.Dodge(by = ['epochs',"discriminator","batch"]),
                  margin_titles=True, despine=False, legend_out=True,
                  sharex=False, sharey=False)
g.map(sns.scatterplot, 'Value', 'Count', 'epochs',"discriminator","batch")
g.set_titles(col_template="{col_name}", row_template="{row_name}")
g.set_axis_labels("", "Count")
g.fig.tight_layout()
plt.legend(loc="lower center", frameon=False, bbox_to_anchor=(-.75, -.5), ncol=10)

# Show the plot
plt.show()
# plt.savefig(os.path.join(graphs, "graph_ctgan_categorical.pdf"), format="pdf", bbox_inches='tight')
