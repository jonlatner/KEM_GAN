'''
TOP COMMANDS
'''

# load libraries
import os
import pandas as pd
import numpy as np
import random
import time

from DataSynthesizer.DataDescriber import DataDescriber
from DataSynthesizer.DataGenerator import DataGenerator

# file paths - adapt main_dir pathway
#main_dir = "N:/Ablagen/D01700-KEM/Latner/simulation/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/simulation_data/continuous/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/"
tables = "tables/"

os.chdir(main_dir)

# beginning commands
pd.set_option('display.width', 1000)
pd.set_option('display.float_format', '{:.2f}'.format)

# Set the random seed
my_seed = 1233

'''
LOAD DATA
'''

filename = 'ods_0.csv'
description = 'description.json'

filename_path = os.path.join(original_data, filename)

df_ods = pd.read_csv(filename_path, index_col=False)
df_ods.info()
df_ods.describe()

'''
Step 2 user-defined parameteres
'''

# input dataset
input_data = filename_path

# An attribute is categorical if its domain size is less than this threshold.
# Here modify the threshold to adapt to the domain size of "education" (which is 14 in input dataset).
print(df_ods.nunique())
threshold_value = 25

# location of two output files
description_file = os.path.join(synthetic_data, description)

# Number of tuples generated in synthetic dataset.
num_tuples_to_generate = len(df_ods.index) 

'''
Step 3 DataDescriber

    a) Instantiate a DataDescriber.
    b) Compute the statistics of the dataset.
    c) Save dataset description to a file on local machine.
'''

"""
DataSynthesizer
Experiments were performed using XX parameters: 
    1) differential privacy (epsilon, ϵ) parameters.
    2) network degree
    3) the number of copies
"""

# Iterate over the values
parents = [0, 1, 2] # The maximum number of parents in Bayesian network, i.e., the maximum number of incoming edges. 
privacy = [0, .1, 1, 5, 10, 20, 30] # A parameter in Differential Privacy. 
copies = [1, 5, 10]

parents = [0] # The maximum number of parents in Bayesian network, i.e., the maximum number of incoming edges. 
privacy = [0] # A parameter in Differential Privacy. 
copies = [1]

for e in privacy:
    for p in parents:
        for m in copies: 

            time.sleep(5)
            
            for j in range(m):    

                j = j + 1

                my_seed = my_seed + 1
                np.random.seed(my_seed)
                random.seed(my_seed)

                describer = DataDescriber(category_threshold=threshold_value)
                describer.describe_dataset_in_correlated_attribute_mode(dataset_file=input_data,
                                                                        epsilon=e,
                                                                        k=p,
                                                                        seed=my_seed)  
                describer.save_dataset_description_to_file(description_file)
                
                '''
                Step 4 generate synthetic dataset
                
                    a) Instantiate a DataGenerator.
                    b) Generate a synthetic dataset.
                    c) Save it to local machine.
                '''

                # Create a unique filename based on the values
                filename = f"sds_datasynthesizer_tuning_e_{e}_k_{p}_m_{m}_n_{j}.csv"
        
                # Construct the full file path using os.path.join()
                full_path = os.path.join(synthetic_data, filename)
        
                generator = DataGenerator()
                generator.generate_dataset_in_correlated_attribute_mode(num_tuples_to_generate, description_file)
                generator.save_synthetic_data(full_path)

                print("privacy:",e)
                print("parents:",p)
                print("copies:",m)
                print("number:",j)
                
                
