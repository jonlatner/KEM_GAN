'''
TOP COMMANDS
'''

# load libraries
import os
import pandas as pd
import numpy as np
import random

from DataSynthesizer.DataDescriber import DataDescriber
from DataSynthesizer.DataGenerator import DataGenerator

# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"
data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"

os.chdir(main_dir)

# beginning commands
pd.set_option('display.width', 1000)
pd.set_option('display.float_format', '{:.2f}'.format)

# Set the random seed
my_seed = 1234

'''
LOAD DATA
'''

df_frequency = pd.DataFrame()
data = ["simulated"] # Real data
copies = [10]
parents = [4]
epsilon = [0,.1,.25,.5,.75,1]

for d in data:
    for m in copies:
        for p in parents:
            for e in epsilon:
                for j in range(m):    
                    
                    j = j + 1
        
                    my_seed = my_seed + 1
                    np.random.seed(my_seed)
                    random.seed(my_seed)
        
                    # Create a unique filename based on the values
                    filename_ods = f"{d}.csv"
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
                        
                    '''
                    Step 3 DataDescriber
                    '''
                    # 3a. Instantiate a DataDescriber.
                    describer = DataDescriber(category_threshold=threshold_value)
        
                    # 3b. Compute the statistics of the dataset.
                    describer.describe_dataset_in_correlated_attribute_mode(dataset_file=input_data,
                                                                            k=p,
                                                                            epsilon=e,
                                                                            seed=my_seed)  
                    
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
                    filename_sds = f"sds_datasynthesizer.csv"
                    # Construct the full file path using os.path.join()
                    full_path_sds = os.path.join(synthetic_data, filename_sds)
                    generator.save_synthetic_data(full_path_sds)

                    df_sds = pd.read_csv(full_path_sds)
   
                    # Assuming df_ods is already defined
                    frequency = df_sds.copy()

                    # Combine columns var1, var2, var3, and var4 into a new column 'combine'
                    frequency['combine'] = frequency['var1'].astype(str) + frequency['var2'].astype(str) + frequency['var3'].astype(str) + frequency['var4'].astype(str)

                    # Drop columns that match the pattern 'var'
                    frequency = frequency.drop(columns=[col for col in frequency.columns if 'var' in col])

                    # Create a frequency table
                    frequency = frequency['combine'].value_counts().reset_index()
                    frequency.columns = ['combine', 'Freq']

                    # Calculate the percentage
                    frequency['pct'] = (frequency['Freq'] / len(df_ods)) * 100
                    frequency['epsilon'] = e
                    frequency['copies'] = m
                    frequency['number'] = j
                    frequency['parents'] = p

                    # Append data
                    df_frequency = pd.concat([df_frequency, frequency], ignore_index=True)

                    print("parents:", p)
                    print("epsilon:", e)
                    print("copes:", m)
                    print("number:", j)

df_frequency = df_frequency.astype(str)
full_path = os.path.join(synthetic_data, 'datasynthesizer_frequency.csv')
df_frequency.to_csv(full_path, index=False)


