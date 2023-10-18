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
main_dir = "/Users/jonathanlatner/Documents/GitHub/IAB/simulation_data/categorical_dim/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/"
tables = "tables/"
duration = "duration/"

os.chdir(main_dir)

# beginning commands
pd.set_option('display.width', 1000)
pd.set_option('display.float_format', '{:.2f}'.format)

# Set the random seed
my_seed = 1233

'''
LOAD DATA
'''


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
rows = [1000, 5000] # Rows/observations
cols = [10, 15, 20] # Columns/variables
vals = [5, 10, 15, 20]  # Number of possible options for each character

m = 1

df_duration = pd.DataFrame(columns=["type", "rows", "cols", "vals", "n", "duration"])

for r in rows:
    for c in cols:
        for v in vals:

            # Start the timer
            time_start = time.time()

            for j in range(m):    

                j = j + 1

                my_seed = my_seed + 1
                np.random.seed(my_seed)
                random.seed(my_seed)

                # load data
                filename_ods = f"ods_rows_{r}_cols_{c}_vals_{v}.csv"
                description = 'description.json'
                
                filename_path = os.path.join(original_data, filename_ods)
                df_ods = pd.read_csv(filename_path, index_col=False)
                
                # input dataset
                input_data = filename_path
                
                # An attribute is categorical if its domain size is less than this threshold.
                # Here modify the threshold to adapt to the domain size of "education" (which is 14 in input dataset).
                threshold_value = 25
                
                # location of two output files
                description_file = os.path.join(synthetic_data, description)
                
                # Number of tuples generated in synthetic dataset.
                num_tuples_to_generate = len(df_ods.index) 
                
                describer = DataDescriber(category_threshold=threshold_value)
                describer.describe_dataset_in_correlated_attribute_mode(dataset_file=input_data,
                                                                        epsilon=0,
                                                                        k=2,
                                                                        seed=my_seed)  
                describer.save_dataset_description_to_file(description_file)
                
                '''
                Step 4 generate synthetic dataset
                
                    a) Instantiate a DataGenerator.
                    b) Generate a synthetic dataset.
                    c) Save it to local machine.
                '''

                # Create a unique filename based on the values
                filename_sds = f"sds_datasynthesizer_rows_{r}_cols_{c}_vals_{v}_n_{j}.csv"
        
                # Construct the full file path using os.path.join()
                full_path_sds = os.path.join(synthetic_data, filename_sds)
        
                generator = DataGenerator()
                generator.generate_dataset_in_correlated_attribute_mode(num_tuples_to_generate, description_file)
                generator.save_synthetic_data(full_path_sds)
                
                print("rwos:",r)
                print("cols:",c)
                print("vals:",v)

            # Stop the timer
            time_end = time.time()
                
            # Calculate the execution time for this iteration
            time_duration = time_end - time_start
        
            output = ("datasynthesizer",r,c,v,j,time_duration)
            df_output = pd.DataFrame([output], columns=["type", "rows", "cols", "vals", "n", "duration"])
            df_duration = pd.concat([df_duration, df_output], ignore_index=True)
            
                
print(df_duration)

filename_dur = "duration_datasynthesizer.csv"
df_duration.to_csv(os.path.join(duration, filename_dur), index=False)
