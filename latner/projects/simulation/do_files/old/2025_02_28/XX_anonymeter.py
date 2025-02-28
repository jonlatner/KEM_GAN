# load libraries
import os
import pandas as pd
import anonymeter

# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"
data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"

os.chdir(main_dir)

# beginning commands
pd.set_option('display.width', 1000)
pd.set_option('display.float_format', '{:.2f}'.format)

# Set the random seed
my_seed = 1234

# Load the data

df_ods = pd.read_csv(os.path.join(original_data, "simulated.csv"), index_col=False)
df_sds = pd.read_csv(os.path.join(synthetic_data, "synthpop_cart.csv"), index_col=False)

evaluator = Evaluator(ori: df_ods, 
                      syn: df_sds, 
                      control: df_ods, 
                      n_attacks: int)

evaluator.evaluate()
risk = evaluator.risk()