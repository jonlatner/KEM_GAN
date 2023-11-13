'''
TOP COMMANDS
Create data for:
https://arxiv.org/abs/1907.00503
Modeling Tabular Data using Conditional GAN
Xu et al., 2019
'''

# load libraries
import os
from sdv.datasets.demo import download_demo
from sdv.datasets.demo import get_available_demos

# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/simulation_data/benchmark/"
data_files = "data_files/"
original_data = "data_files/original/"

os.chdir(main_dir)

'''
LOAD DATA
'''

# https://docs.sdv.dev/sdv/multi-table-data/data-preparation/loading-data#get_available_demos
get_available_demos(modality='single_table')

# i select a few small datasets
data = ["grid","gridr", #Simulated data
        "census"] # Real data

for d in data:

    data, metadata = download_demo(
        modality='single_table',
        dataset_name=d)
    
    # Create a unique filename based on the values
    filename = f"{d}.csv"
    
    data.to_csv(os.path.join(original_data, filename), index=False)

