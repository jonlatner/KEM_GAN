'''
TOP COMMANDS
'''

# load libraries
import os
import pandas as pd
import numpy as np
import random
import time

from DataSynthesizer.DataDescriber import DataDescriber
from DataSynthesizer.DataGenerator import DataGenerator

# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"
data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"
duration_folder = "duration/SD2011/"

os.chdir(main_dir)

# beginning commands
pd.set_option('display.width', 1000)
pd.set_option('display.float_format', '{:.2f}'.format)

# Set the random seed
my_seed = 1233

'''
LOAD DATA
'''


'''
Step 3 DataDescriber

    a) Instantiate a DataDescriber.
    b) Compute the statistics of the dataset.
    c) Save dataset description to a file on local machine.
'''

"""
DataSynthesizer
Experiments were performed using XX parameters: 
    1) differential privacy (epsilon, ϵ) parameters.
    2) network degree
    3) the number of copies
"""

data = ["sd2011"] # Real data
copies = [1]
parents = [1,2]
epsilon = [0,.1,1]
df_duration = [] # empty data frame

# save smaller files with and without missing
filename_path = os.path.join(original_data, "sd2011.csv")
df_ods = pd.read_csv(filename_path, index_col=False)
df_ods = df_ods[['age','eduspec','sex','alcabuse']]
filename_duration = "sd2011_duration_w_missing.csv"
df_ods.to_csv(os.path.join(original_data, filename_duration), index=False)


filename_path = os.path.join(original_data, "sd2011.csv")
df_ods = pd.read_csv(filename_path, index_col=False)
df_ods = df_ods[['age','eduspec','sex','alcabuse']]
df_ods = df_ods.dropna()
filename_duration = "sd2011_duration_wo_missing.csv"
df_ods.to_csv(os.path.join(original_data, filename_duration), index=False)

# With missing data
for d in data:
    for m in copies:
        for p in parents:
            for e in epsilon:
                for j in range(m):    
                    
                    j = j + 1
        
                    my_seed = my_seed + 1
                    np.random.seed(my_seed)
                    random.seed(my_seed)
        
                    # Create a unique filename based on the values
                    filename_ods = "sd2011_duration_w_missing.csv"
                    description = 'description.json'
                        
                    filename_path = os.path.join(original_data, filename_ods)
                    df_ods = pd.read_csv(filename_path, index_col=False)
                    df_ods = df_ods[['age','eduspec','sex','alcabuse']]
                        
                    # input dataset
                    input_data = filename_path
                        
                    # An attribute is categorical if its domain size is less than this threshold.
                    # Here modify the threshold to adapt to the domain size of "education" (which is 14 in input dataset).
                    threshold_value = 25
                        
                    # location of two output files
                    description_file = os.path.join(synthetic_data, description)
                        
                    # Number of tuples generated in synthetic dataset.
                    num_tuples_to_generate = len(df_ods.index) 
                        
                    '''
                    Step 3 DataDescriber
                    '''
                    # 3a. Instantiate a DataDescriber.
                    describer = DataDescriber(category_threshold=threshold_value)
        
                    # Start the timer
                    time_start = time.time()
                    
                    # 3b. Compute the statistics of the dataset.
                    describer.describe_dataset_in_correlated_attribute_mode(dataset_file=input_data,
                                                                            k=p,
                                                                            epsilon=e,
                                                                            seed=my_seed)  
        
                    # Stop the timer
                    time_end = time.time()
                        
                    print("")
                    print("data:",d)
                    print("parents:",p)
                    print("epsilon:",e)
                    print("copes:",m)
                    print("number:",j)
                        
                    # Calculate the execution time for this iteration
                    time_duration = time_end - time_start
                    df_duration.append(["datasynthesizer","SD2011","yes",p,e,m,j,time_duration])
        
# Without missing data
for d in data:
    for m in copies:
        for p in parents:
            for e in epsilon:
                for j in range(m):    
                    
                    j = j + 1
        
                    my_seed = my_seed + 1
                    np.random.seed(my_seed)
                    random.seed(my_seed)
        
                    # Create a unique filename based on the values
                    filename_ods = "sd2011_duration_wo_missing.csv"
                    description = 'description.json'
                        
                    filename_path = os.path.join(original_data, filename_ods)
                    df_ods = pd.read_csv(filename_path, index_col=False)
                    df_ods = df_ods[['age','eduspec','sex','alcabuse']]

                    # Drop missing values from the DataFrame
                    df_ods = df_ods.dropna()

                    # input dataset
                    input_data = filename_path
                        
                    # An attribute is categorical if its domain size is less than this threshold.
                    # Here modify the threshold to adapt to the domain size of "education" (which is 14 in input dataset).
                    threshold_value = 25
                        
                    # location of two output files
                    description_file = os.path.join(synthetic_data, description)
                        
                    # Number of tuples generated in synthetic dataset.
                    num_tuples_to_generate = len(df_ods.index) 
                        
                    '''
                    Step 3 DataDescriber
                    '''
                    # 3a. Instantiate a DataDescriber.
                    describer = DataDescriber(category_threshold=threshold_value)
        
                    # Start the timer
                    time_start = time.time()
                    
                    # 3b. Compute the statistics of the dataset.
                    describer.describe_dataset_in_correlated_attribute_mode(dataset_file=input_data,
                                                                            k=p,
                                                                            epsilon=e,
                                                                            seed=my_seed)  
        
                    # Stop the timer
                    time_end = time.time()
                        
                    print("")
                    print("data:",d)
                    print("parents:",p)
                    print("epsilon:",e)
                    print("copes:",m)
                    print("number:",j)
                        
                    # Calculate the execution time for this iteration
                    time_duration = time_end - time_start
                    df_duration.append(["datasynthesizer","SD2011","no",p,e,m,j,time_duration])
        
            # save duration by data set
            df_duration_out = pd.DataFrame(df_duration)
            df_duration_out.columns = ['type', 'data', "missing", 'parents','epsilon', "copies", "j", "duration"]    
            filename_duration = "duration_datasynthesizer_correlated_data.csv"
            df_duration_out.to_csv(os.path.join(duration_folder, filename_duration), index=False)

df_duration_out

