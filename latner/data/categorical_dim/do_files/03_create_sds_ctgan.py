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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/categorical_dim/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
duration = "duration/"

os.chdir(main_dir)

# beginning commands
pd.set_option('display.width', 1000)
pd.set_option('display.float_format', '{:.2f}'.format)

# Set the random seed
my_seed = 1230

os.chdir(main_dir)

'''
Run synthesizer
'''

# Dimensions
rows = [1000,5000] # Rows/observations
cols = [10,15,20] # Columns/variables
vals = [5,10,15,20]  # Number of possible options for each character

epochs = [10,20,30,40,50,75,100,300]  # Number of possible options for each character

m = 1

df_duration = pd.DataFrame(columns=["type", "rows", "cols", "vals", "n", "duration","epochs"])


for r in rows:
    for c in cols:
        for v in vals:
            for e in epochs:

                # load data
                filename_ods = f"ods_rows_{r}_cols_{c}_vals_{v}.csv"
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
                    synthesizer = CTGANSynthesizer(metadata,
                                                   epochs=e,
                                                   verbose=True)
                                    
                    # Step 2: Train the synthesizer
                    synthesizer.fit(df_ods)
                                        
                    # Step 3: Generate synthetic data set (sds)
                    sds = synthesizer.sample(num_rows=len(df_ods.index))
            
                    # Create a unique filename based on the values
                    filename_sds = f"sds_ctgan_rows_{r}_cols_{c}_vals_{v}_n_{j}_epochs_{e}.csv"
                    
                    # Step 4: Save synthetic data set (sds)
                    sds.to_csv(os.path.join(synthetic_data, filename_sds), index=False)
        
                    print("")
                    print("rows:",r)
                    print("cols:",c)
                    print("epochs:",e)
                    print("vals:",v)
                    print("number:",j)
        
                # Stop the timer
                time_end = time.time()
                    
                # Calculate the execution time for this iteration
                time_duration = time_end - time_start
                
                output = ("ctgan",r,c,vals,j,time_duration,e)
                df_output = pd.DataFrame([output], columns=["type", "rows", "cols", "n", "duration","epochs"])
                df_duration = pd.concat([df_duration, df_output], ignore_index=True)

print(df_duration)
    
filename_dur = "duration_ctgan_compare_epochs.csv"
df_duration.to_csv(os.path.join(duration, filename_dur), index=False)

