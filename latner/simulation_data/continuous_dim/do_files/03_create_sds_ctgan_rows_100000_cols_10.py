'''
TOP COMMANDS
'''

# load libraries
## Basics 
import os
import pandas as pd
import numpy as np
import random
import time

from sdv.metadata import SingleTableMetadata
from sdv.single_table import CTGANSynthesizer

# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/IAB/simulation_data/continuous_dim/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
duration = "duration/"

os.chdir(main_dir)

# beginning commands
pd.set_option('display.width', 1000)
pd.set_option('display.float_format', '{:.2f}'.format)
pd.set_option('display.max_columns', None)

# Set the random seed
my_seed = 1230

os.chdir(main_dir)

'''
Run synthesizer
'''

# Dimensions
rows = [100000] # Rows/observations
cols = [10] # Columns/variables
epochs = [10, 20, 30, 40, 50, 75, 100]
batch = [500, 1000, 5000, 10000]

m = 1

df_duration = pd.DataFrame(columns=["type", "rows", "cols", "n", "duration", "epochs"])

for r in rows:
    for c in cols:
        for e in epochs:
            for b in batch:

                # load data
                filename_ods = f"ods_rows_{r}_cols_{c}.csv"
                df_ods = pd.read_csv(os.path.join(original_data, filename_ods), index_col=False)
                    
                # synthesize data
                metadata = SingleTableMetadata()
                metadata.detect_from_dataframe(data=df_ods)
            
                # Start the timer
                time_start = time.time()
            
                for j in range(m):    
                        
                    j = j + 1
                        
                    my_seed = my_seed + 1
                    np.random.seed(my_seed)
                    random.seed(my_seed)
                    
                    # Step 1: Create the synthesizer
                    # batch = int(r*b)
                    
                    synthesizer = CTGANSynthesizer(metadata,
                                                   batch_size=b, 
                                                   epochs = e, 
                                                   verbose=True)
                                        
                    # Step 2: Train the synthesizer
                    synthesizer.fit(df_ods)
                                            
                    # Step 3: Generate synthetic data set (sds)
                    sds = synthesizer.sample(num_rows=len(df_ods.index))
                
                    # Create a unique filename based on the values
                    filename_sds = f"sds_ctgan_rows_{r}_cols_{c}_n_{j}_epochs_{e}_batch_{b}.csv"
                        
                    # Step 4: Save synthetic data set (sds)
                    sds.to_csv(os.path.join(synthetic_data, filename_sds), index=False)
            
                    # Stop the timer
                    time_end = time.time()
                        
                    # Calculate the execution time for this iteration
                    time_duration = time_end - time_start
                
                    output = ("ctgan",r,c,j,time_duration,b,e)
                    df_output = pd.DataFrame([output], columns=["type", "rows", "cols", "n", "duration","batch_size","epochs"])
                    df_duration = pd.concat([df_duration, df_output], ignore_index=True)
        
                    print("rows:",r)
                    print("cols:",c)
                    print("epochs:",e)
                    print("number:",j)
                    print("batch size:",b)
                    print("duration:",time_duration)
        
filename_dur = "duration_ctgan_rows_100000_cols_10.csv"
df_duration.to_csv(os.path.join(duration, filename_dur), index=False)

print(df_duration)

