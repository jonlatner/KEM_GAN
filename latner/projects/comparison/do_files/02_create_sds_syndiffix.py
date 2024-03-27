'''
TOP COMMANDS
'''

# load libraries
import os
import pandas as pd
import numpy as np
import random
import time
from syndiffix import Synthesizer


# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"
data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/syndiffix/"
duration = "duration/"

os.chdir(main_dir)

# Set the random seed
my_seed = 1233

'''
LOAD DATA
'''

data = ["sd2011_clean_small"] # Real data
copies = [5]

for d in data:
    df_duration = [] # empty data frame

    for m in copies:
        my_seed = my_seed + 1
        
        for j in range(m):
            j = j + 1

            my_seed = my_seed + 1
            np.random.seed(my_seed)
            random.seed(my_seed)

            # Create a unique filename based on the values
            filename_ods = f"{d}.csv"
            df_ods = pd.read_csv(os.path.join(original_data, filename_ods))
            
            # Start the timer
            time_start = time.time()

            sds = Synthesizer(df_ods).sample()
            
            # Stop the timer
            time_end = time.time()
                
            # Calculate the execution time for this iteration
            time_duration = time_end - time_start
                
            # Store duration values
            df_duration.append(["syndiffix",d,m,j,time_duration])

            # Create a unique filename based on the values
            filename_sds = f"sds_syndiffix_data_{d}_m_{m}_n_{j}.csv"
                            
            # Step 4: Save synthetic data set (sds)
            sds.to_csv(os.path.join(synthetic_data, filename_sds), index=False)
            
            print("")
            print("data:",d)
            print("copes:",m)
            print("number:",j)
            print("duration:",time_duration)
            
    # save duration by data set
    df_duration_out = pd.DataFrame(df_duration)
    df_duration_out.columns = ['type', 'data', "copies", "j", "duration"]    
    filename_duration = f"duration_syndiffix_data_{d}.csv"
    df_duration_out.to_csv(os.path.join(duration, filename_duration), index=False)

            
            
