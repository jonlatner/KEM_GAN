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
duration = "duration/"

os.chdir(main_dir)

# Set the random seed
my_seed = 1233

'''
LOAD DATA
'''

data = ["grid","gridr", # Simulated data
        "adult","sd2011_small"] # Real data
data = ["sd2011_duration_w_missing","sd2011_duration_wo_missing"] # Real data
epochs = [25,50,75,100]
copies = [1,5]


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
        
                # access loss values from the ML model directly
                # https://github.com/sdv-dev/SDV/issues/1671
                loss_values = synthesizer._model.loss_values
                
                loss_values_long = pd.melt(
                    loss_values,
                    id_vars=['Epoch'],
                    var_name='Loss Type',
                    value_name='Value'
                )
                loss_values_long['Value'] = loss_values_long['Value'].astype(str).astype(float)
                loss_values_long['data']=d
                loss_values_long['m']=m
                loss_values_long['j']=j
                loss_values_long = pd.DataFrame(loss_values_long)
                df_loss_values = pd.concat([df_loss_values,loss_values_long], ignore_index=True)
                
                print("")
                print("data:",d)
                print("epochs:",e)
                print("copes:",m)
                print("number:",j)

        # save loss values by data set and copy
        df_loss_values_out = pd.DataFrame(df_loss_values)
        df_loss_values_out.columns = ['epochs', 'loss_type', 'value', "data", "copes", "j"]    
        filename_loss_values = f"loss_values_ctgan_data_{d}_m_{m}.csv"
        df_loss_values_out.to_csv(os.path.join(duration, filename_loss_values), index=False)
    
    # save duration by data set
    df_duration_out = pd.DataFrame(df_duration)
    df_duration_out.columns = ['type', 'data', 'epochs', "copies", "j", "duration"]    
    filename_duration = f"duration_ctgan_data_{d}.csv"
    df_duration_out.to_csv(os.path.join(duration, filename_duration), index=False)


# Plot the data
# import seaborn as sns
# import matplotlib.pyplot as plt
# df_graph = loss_values_long.copy()
# sns.lineplot(data=df_graph, x='Epoch', y = 'Value', hue='Loss Type')
# plt.legend()
# plt.title('Generator vs Discriminator Loss over Epochs')
# plt.show()
