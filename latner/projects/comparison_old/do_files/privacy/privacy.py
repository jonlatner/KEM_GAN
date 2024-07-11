'''
TOP COMMANDS
'''

# load libraries
import os
import pandas as pd
import sys

sys.path.append('/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/do_files/privacy/')
from TCAP_risk_calculations_JPL import tcap

# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"
data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
duration = "duration/"

os.chdir(main_dir)

# beginning commands
pd.set_option('display.float_format', '{:.2f}'.format)
pd.set_option('display.max_columns', None)
pd.set_option('display.width', 100)

# Set the random seed
my_seed = 1234


'''
LOAD DATA
'''

df_ods = pd.read_csv(os.path.join(original_data, "sd2011_clean_small.csv"), index_col=False)

# df_ods.head(5)
# df_ods.describe(include='all')
# print(df_ods['edu'].value_counts(normalize=True,dropna=False))

ods = os.path.join(main_dir,original_data, "sd2011_clean_small.csv")
synth = os.path.join(main_dir,synthetic_data, "datasynthesizer/sds_datasynthesizer_sd2011_clean_small_k_2_e_0_m_5_n_1.csv")
synth = os.path.join(main_dir,synthetic_data, "synthpop/sds_synthpop_sd2011_clean_small_m_5_n_1.csv")

keys = ["sex","edu","age","marital","region"]
keys = ["sex","edu","age","marital"]
keys = ["sex","edu","age"]
target = "income"
num_keys = len(keys)

output = tcap(original=ods, synth=synth, target=target, keys=keys, num_keys=num_keys, verbose = False)

print("")
print("Number of keys",num_keys)
print("TCAP",round(output[0],3))
print("Baseline",round(output[1],3))
