'''
TOP COMMANDS
'''

# load libraries
import os
import pandas as pd
import time

from sdv.single_table import CTGANSynthesizer
from sdv.metadata import SingleTableMetadata


# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/ctgan/"
data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
duration = "duration/"

os.chdir(main_dir)

'''
Vary dimensions
'''

data = ["sd2011_clean_small"] # Real data

generator  = [1e-6] 
discriminator = [2e-6]

for df in data:
    df_duration = [] # empty data frame
    for g in generator:
        for d in discriminator:
            # Create a unique filename based on the values
            filename_ods = f"{df}.csv"
            df_ods = pd.read_csv(os.path.join(original_data, filename_ods))
            
            # synthesize data
            metadata = SingleTableMetadata()
            metadata.detect_from_dataframe(data=df_ods)
                
            # Step 1: Create the synthesizer
            synthesizer = CTGANSynthesizer(metadata, 
                                           generator_lr = g,
                                           discriminator_lr = d,
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
            # df_duration.append(["ctgan",df,e,d,time_duration])
       
            # Step 3: Generate synthetic data set (sds)
            sds = synthesizer.sample(num_rows=len(df_ods.index))
        
            # Create a unique filename based on the values
            filename_sds = f"sds_ctgan_data_{df}_learning_rate.csv"
                            
            # Step 4: Save synthetic data set (sds)
            sds.to_csv(os.path.join(synthetic_data, filename_sds), index=False)
