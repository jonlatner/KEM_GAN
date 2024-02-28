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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/datasynthesizer/"
data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"
duration = "duration/"

os.chdir(main_dir)

# beginning commands
pd.set_option('display.width', 1000)
pd.set_option('display.float_format', '{:.2f}'.format)

# Set the random seed
my_seed = 1234
random.seed(my_seed)

'''
LOAD DATA
'''

from DataSynthesizer.DataDescriber import DataDescriber
from DataSynthesizer.DataGenerator import DataGenerator
from DataSynthesizer.ModelInspector import ModelInspector
from DataSynthesizer.lib.utils import read_json_file, display_bayesian_network

import pandas as pd

# input dataset

# Create a unique filename based on the values
filename_ods = "ods_vars_4_vals_2.csv"
description = 'description.json'
                        
filename_path = os.path.join(original_data, filename_ods)
df_ods = pd.read_csv(filename_path, index_col=False)
                        
# input dataset
input_data = filename_path
                        
# An attribute is categorical if its domain size is less than this threshold.
# Here modify the threshold to adapt to the domain size of "education" (which is 14 in input dataset).
threshold_value = 30
                        
# location of two output files
description_file = os.path.join(synthetic_data, description)
                        
# Number of tuples generated in synthetic dataset.
num_tuples_to_generate = len(df_ods.index) 


threshold_value = 20

e = 0

# The maximum number of parents in Bayesian network, i.e., the maximum number of incoming edges.
degree_of_bayesian_network = 2

'''
Step 3 DataDescriber
'''
# 3a. Instantiate a DataDescriber.
describer = DataDescriber(category_threshold=threshold_value)
        
# Start the timer
time_start = time.time()
                    
# 3b. Compute the statistics of the dataset.
describer.describe_dataset_in_correlated_attribute_mode(dataset_file=input_data,
                                                        k=degree_of_bayesian_network,
                                                        epsilon=e,
                                                        seed=my_seed)  
        
# Stop the timer
time_end = time.time()
        
# 3c. Save dataset description to a file on local machine.
describer.save_dataset_description_to_file(description_file)

'''
Step 4 generate synthetic dataset
'''
        
# 4a. Instantiate a DataGenerator.
generator = DataGenerator()
                    
# 4b. Generate a synthetic dataset.
generator.generate_dataset_in_correlated_attribute_mode(num_tuples_to_generate, description_file)
                    
# 4c. Save it to local machine.
# Create a unique filename based on the values
filename_sds = "sds_datasynthesizer_ods_vars_4_vals_2.csv"
# Construct the full file path using os.path.join()
full_path_sds = os.path.join(synthetic_data, filename_sds)
generator.save_synthetic_data(full_path_sds)


# Read both datasets using Pandas.
input_df = pd.read_csv(input_data, skipinitialspace=True)
synthetic_df = pd.read_csv(full_path_sds)
# Read attribute description from the dataset description file.
attribute_description = read_json_file(description_file)['attribute_description']

inspector = ModelInspector(input_df, synthetic_df, attribute_description)

for attribute in synthetic_df.columns:
    inspector.compare_histograms(attribute)
    
inspector.mutual_information_heatmap()

