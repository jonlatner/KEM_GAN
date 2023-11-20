'''
Top commands
'''

# load packages
import os
import pandas as pd
import numpy as np
import random
import pickle

# file paths - adapt main_dir pathway
#main_dir = "N:/Ablagen/D01700-KEM/Latner/simulation/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/simulation_cat/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

os.chdir(main_dir)

# Set the random seed
np.random.seed(1234)
random.seed(1234)

'''
Create data
'''

# Initialize an empty list to store the data frames
# Step 1: Generate 5 data frames with randomly generated variables
copies = [2, 3, 4]

for m in copies:
    
    data_frames_list = []

    for j in range(m):    
        # Set the number of observations
        n = 10
        
        # Create an empty dictionary to store the data
        data = {}
        for i in range(1, 5):  # 4 dichotomous variables
            variable_name = f'var_{i}'
            data[variable_name] = [random.choice(["A", "B"]) for _ in range(n)]
    
            # Create the data frame from the random data
            df = pd.DataFrame(data)
        
        # Append the data frame to the list
        data_frames_list.append(df)
    
        # Step 2: Save the list of data frames to a pickle file
        filename = f"random_data_frames_m_{m}.pkl"
        with open(os.path.join(synthetic_data,filename), 'wb') as f:
            pickle.dump(data_frames_list, f)
    


# Step 3: Open the pickle file in read-binary mode ('rb')
with open(os.path.join(synthetic_data,'random_data_frames_m_3.pkl'), 'rb') as f:
    # Step 3: Load the data frames from the pickle file
    data_frames_list = pickle.load(f)
    
len(data_frames_list)

df0_loaded = data_frames_list[0]
df0_loaded

df1_loaded = data_frames_list[1]
df1_loaded
