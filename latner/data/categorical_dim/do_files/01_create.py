'''
Top commands
'''

# load packages
import os
import pandas as pd
import numpy as np
import random
import string

# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/categorical_dim/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

os.chdir(main_dir)

# Set the random seed
np.random.seed(1234)
random.seed(1234)

'''
Create data
'''

# Dimensions
rows = [100000] # Rows/observations
cols = [10,15,20] # Columns/variables
vals = [5,10]  # Number of possible options for each character

df_output = pd.DataFrame()
df_sds = pd.DataFrame()

# Set the random seed
my_seed = 1233

for r in rows:
    for c in cols:
        for v in vals:
            
            my_seed = my_seed + 1
            
            # Create an empty dictionary to store the data
            data = {}
            
            # Generate data for each variable
            for i in range(1, c+1):  # 4 dichotomous variables
                variable_name = f'var_{i}'
                data[variable_name] = [random.choice(string.ascii_letters[:v]) for _ in range(r)]
    
            # Create the DataFrame original data set (ods)
            df_ods = pd.DataFrame(data)
    
            '''
            Save
            '''
    
            # Create a unique filename based on the values
            filename = f"ods_rows_{r}_cols_{c}_vals_{v}.csv"
    
            df_ods.to_csv(os.path.join(original_data, filename), index=False)
            
            print("rwos:",r)
            print("cols:",c)
            print("vals:",v)
