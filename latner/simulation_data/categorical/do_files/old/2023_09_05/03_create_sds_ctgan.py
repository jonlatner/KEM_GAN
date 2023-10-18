'''
TOP COMMANDS
https://docs.sdv.dev/sdv/single-table-data/modeling/synthesizers/ctgansynthesizer
https://sdv.dev/SDV/user_guides/single_table/ctgan.html#advanced-usage
https://docs.sdv.dev/sdv/single-table-data/modeling/synthesizers/ctgansynthesizer
https://datacebo.com/blog/interpreting-ctgan-progress/

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
#main_dir = "N:/Ablagen/D01700-KEM/Latner/simulation/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/simulation_cat/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/"
tables = "tables/"

os.chdir(main_dir)

# beginning commands
pd.set_option('display.width', 1000)
pd.set_option('display.float_format', '{:.2f}'.format)

# Set the random seed
my_seed = 1233

os.chdir(main_dir)

'''
LOAD DATA
'''

df_ods = pd.read_csv(os.path.join(original_data,"ods_0.csv"), index_col=False)
df_ods.info()

'''
Synthesize data
'''

metadata = SingleTableMetadata()
metadata.detect_from_dataframe(data=df_ods)
python_dict = metadata.to_dict()

"""
CTGAN synthesizer
Experiments were performed using XX parameters: 
    1) the number of epochs
    2) number of discriminator steps
    3) batch size
    4) log frequency parameters
    5) the number of copies
"""

epochs = [300, 600, 900]
discriminator = [1, 5, 10]
batch = [500, 1000]
frequency = [True, False]
copies = [1, 5, 10]

epochs = [600]
discriminator = [10]
batch = [1000]
frequency = [False]
copies = [1]

df_output = pd.DataFrame()
df_sds = pd.DataFrame()

for d in discriminator:
    for e in epochs:
        for b in batch:
            for f in frequency:
                for m in copies: 

                    time.sleep(5)

                    for j in range(m):    

                        j = j + 1

                        my_seed = my_seed + 1
                        np.random.seed(my_seed)
                        random.seed(my_seed)
    
                        # Step 1: Create the synthesizer
                        synthesizer = CTGANSynthesizer(metadata, 
                                                       log_frequency = f,
                                                       discriminator_steps=d,
                                                       epochs=e,
                                                       batch_size=b,
                                                       verbose=True)
                        
                       
                        # Step 2: Train the synthesizer
                        synthesizer.fit(df_ods)
                    
                        # Step 3: Generate synthetic data set (sds)
                        sds = synthesizer.sample(num_rows=len(df_ods.index))
                        
                    
                        # Create a unique filename based on the values
                        filename = f"sds_ctgan_tuning_e_{e}_d_{d}_b_{b}_f_{f}_m_{m}_n_{j}.csv"

                        # Step 4: Save synthetic data set (sds)
                        sds.to_csv(os.path.join(synthetic_data, filename), index=False)
    
                        print("discriminator:",d)
                        print("epochs:",e)
                        print("batch:",b)
                        print("frequency:",f)
                        print("copies:",m)
                        print("number:",j)
