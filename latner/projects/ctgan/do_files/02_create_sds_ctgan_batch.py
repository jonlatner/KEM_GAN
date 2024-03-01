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
Vary batch_size for constant number of steps (3000)
1) mb = 250; epochs = 150
2) mb = 500; epochs = 300 (default)
3) mb = 1000; epochs = 600
'''

data = ["sd2011_clean_small"] # Real data

for d in data:
    df_duration = [] # empty data frame

    ################################
    epochs = [150]
    batch_size = [250]
    for e in epochs:
        for b in batch_size:
            # Create a unique filename based on the values
            filename_ods = f"{d}.csv"
            df_ods = pd.read_csv(os.path.join(original_data, filename_ods))
            
            # synthesize data
            metadata = SingleTableMetadata()
            metadata.detect_from_dataframe(data=df_ods)
                
            # Step 1: Create the synthesizer
            synthesizer = CTGANSynthesizer(metadata, 
                                           epochs = e,
                                           batch_size = b,
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
            df_duration.append(["ctgan",d,e,b,time_duration])
       
            # Step 3: Generate synthetic data set (sds)
            sds = synthesizer.sample(num_rows=len(df_ods.index))
        
            # Create a unique filename based on the values
            filename_sds = f"sds_ctgan_data_{d}_batch_size_{b}_epochs_{e}.csv"
                            
            # Step 4: Save synthetic data set (sds)
            sds.to_csv(os.path.join(synthetic_data, filename_sds), index=False)
        
            print("")
            print("data:",d)
            print("epochs:",e)
            print("batch_size:",b)

    epochs = [300]
    batch_size = [500]
    for e in epochs:
        for b in batch_size:
            # Create a unique filename based on the values
            filename_ods = f"{d}.csv"
            df_ods = pd.read_csv(os.path.join(original_data, filename_ods))
            
            # synthesize data
            metadata = SingleTableMetadata()
            metadata.detect_from_dataframe(data=df_ods)
                
            # Step 1: Create the synthesizer
            synthesizer = CTGANSynthesizer(metadata, 
                                           epochs = e,
                                           batch_size = b,
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
            df_duration.append(["ctgan",d,e,b,time_duration])
       
            # Step 3: Generate synthetic data set (sds)
            sds = synthesizer.sample(num_rows=len(df_ods.index))
        
            # Create a unique filename based on the values
            filename_sds = f"sds_ctgan_data_{d}_batch_size_{b}_epochs_{e}.csv"
                            
            # Step 4: Save synthetic data set (sds)
            sds.to_csv(os.path.join(synthetic_data, filename_sds), index=False)
        
            print("")
            print("data:",d)
            print("epochs:",e)
            print("batch_size:",b)

    epochs = [600]
    batch_size = [1000]
    for e in epochs:
        for b in batch_size:
            # Create a unique filename based on the values
            filename_ods = f"{d}.csv"
            df_ods = pd.read_csv(os.path.join(original_data, filename_ods))
            
            # synthesize data
            metadata = SingleTableMetadata()
            metadata.detect_from_dataframe(data=df_ods)
                
            # Step 1: Create the synthesizer
            synthesizer = CTGANSynthesizer(metadata, 
                                           epochs = e,
                                           batch_size = b,
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
            df_duration.append(["ctgan",d,e,b,time_duration])
       
            # Step 3: Generate synthetic data set (sds)
            sds = synthesizer.sample(num_rows=len(df_ods.index))
        
            # Create a unique filename based on the values
            filename_sds = f"sds_ctgan_data_{d}_batch_size_{b}_epochs_{e}.csv"
                            
            # Step 4: Save synthetic data set (sds)
            sds.to_csv(os.path.join(synthetic_data, filename_sds), index=False)
        
            print("")
            print("data:",d)
            print("epochs:",e)
            print("batch_size:",b)

    # save duration by data set
    df_duration_out = pd.DataFrame(df_duration)
    df_duration_out.columns = ['type', 'data', 'epochs', 'batch_size', "duration"]    
    filename_duration = f"duration_ctgan_data_{d}_batch.csv"
    df_duration_out.to_csv(os.path.join(duration, filename_duration), index=False)

