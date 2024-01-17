'''
TOP COMMANDS
Create data for:
https://arxiv.org/abs/1907.00503
Modeling Tabular Data using Conditional GAN
Xu et al., 2019
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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"
data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
duration_folder = "duration/SD2011/"

os.chdir(main_dir)

# Set the random seed
my_seed = 1233

'''
LOAD DATA
'''

data = ["sd2011"] # Real data
epochs = [25,50,75,100]
copies = [1]

#with missing values
for d in data:
    df_duration = [] # empty data frame

    for m in copies:

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
                df_ods = df_ods[['age','eduspec','sex','alcabuse']]
            
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
                df_duration.append(["ctgan",'SD2011','yes',e,m,j,time_duration])
       
                # Step 3: Generate synthetic data set (sds)
                sds = synthesizer.sample(num_rows=len(df_ods.index))
        
                print("")
                print("data:",d)
                print("epochs:",e)
                print("copes:",m)
                print("number:",j)
   
#without missing values
for d in data:

    for m in copies:

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
                df_ods = df_ods[['age','eduspec','sex','alcabuse']]
                
                # Drop missing values from the DataFrame
                df_ods = df_ods.dropna()
            
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
                df_duration.append(["ctgan",'SD2011','no',e,m,j,time_duration])
       
                # Step 3: Generate synthetic data set (sds)
                sds = synthesizer.sample(num_rows=len(df_ods.index))
        
                print("")
                print("data:",d)
                print("epochs:",e)
                print("copes:",m)
                print("number:",j)
   
    # save duration by data set
    df_duration_out = pd.DataFrame(df_duration)
    df_duration_out.columns = ['type', 'data','missing', 'epochs', "copies", "j", "duration"]    
    filename_duration = "duration_ctgan_data.csv"
    df_duration_out.to_csv(os.path.join(duration_folder, filename_duration), index=False)

df_duration_out

