'''
TOP COMMANDS
'''

from ucimlrepo import fetch_ucirepo, list_available_datasets
import os
import pandas as pd

# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/simulation_data/benchmark/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/"
tables = "tables/"
duration = "duration/"

os.chdir(main_dir)

# beginning commands
pd.set_option('display.width', 1000)
pd.set_option('display.float_format', '{:.2f}'.format)

# Set the random seed
my_seed = 1234

  
'''
LOAD DATA
'''

# check which datasets can be imported
# https://archive.ics.uci.edu/datasets
list_available_datasets()

# fetch dataset 
adult = fetch_ucirepo(id=2) 
  
# data (as pandas dataframes) 
X = adult.data.features 
y = adult.data.targets 
  
# metadata 
print(adult.metadata) 
  
# variable information 
print(adult.variables) 
