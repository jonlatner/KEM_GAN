'''
TOP COMMANDS
'''

# load libraries
import os
import pandas as pd
import numpy as np
import random
import time

from sdv.single_table import CTGANSynthesizer
from sdv.metadata import SingleTableMetadata


# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/destatis/"
data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
duration = "duration/"

os.chdir(main_dir)

# Set the random seed
my_seed = 1233

'''
LOAD DATA
'''

data = ["sd2011_small"] # Real data
epochs = [10]
copies = [1]


for d in data:
    df_duration = [] # empty data frame

    for m in copies:
        df_loss_values = pd.DataFrame() # empty data frame

        my_seed = my_seed + 1
        
        for e in epochs:
            for j in range(m):    
                
                j = j + 1

                my_seed = my_seed + 1
                np.random.seed(my_seed)
                random.seed(my_seed)

                # Create a unique filename based on the values
                filename_ods = f"{d}.csv"
                df_ods = pd.read_csv(os.path.join(original_data, filename_ods))
            
                # synthesize data
                metadata = SingleTableMetadata()
                metadata.detect_from_dataframe(data=df_ods)
                
                # Step 1: Create the synthesizer
                synthesizer = CTGANSynthesizer(metadata, 
                                                epochs = e,
                                                verbose=True)
            
                # Start the timer
                time_start = time.time()
        
                # Step 2: Train the synthesizer
                synthesizer.fit(df_ods)
                                                
                # Stop the timer
                time_end = time.time()
                
                # Calculate the execution time for this iteration
                time_duration = time_end - time_start
                
                # Store duration values
                df_duration.append(["ctgan",d,e,m,j,time_duration])
       
                # Step 3: Generate synthetic data set (sds)
                sds = synthesizer.sample(num_rows=len(df_ods.index))
        
                # Create a unique filename based on the values
                filename_sds = f"sds_ctgan_data_{d}_epochs_{e}_m_{m}_n_{j}.csv"
                            
                # Step 4: Save synthetic data set (sds)
                sds.to_csv(os.path.join(synthetic_data, filename_sds), index=False)
        
                print("")
                print("data:",d)
                print("epochs:",e)
                print("copes:",m)
                print("number:",j)

    # save duration by data set
    df_duration_out = pd.DataFrame(df_duration)
    df_duration_out.columns = ['type', 'data', 'epochs', "copies", "j", "duration"]    
    filename_duration = f"duration_ctgan_data_{d}.csv"
    df_duration_out.to_csv(os.path.join(duration, filename_duration), index=False)
