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
pd.set_option('display.width', 200)

# Set the random seed
my_seed = 1234


'''
Code
'''

# original data
ods = os.path.join(main_dir,original_data, "sd2011_clean_small.csv")

df_ods = pd.read_csv(ods)
df_ods.info()


# synthpop synthetic data
copies = 5+1
sds = []
for m in range(1, copies):
    filename_sds = f"synthpop/sds_synthpop_sd2011_clean_small_m_5_n_{m}.csv"
    df = os.path.join(main_dir,synthetic_data, filename_sds)
    sds.append(df)

    filename_sds = f"datasynthesizer/sds_datasynthesizer_sd2011_clean_small_k_2_e_0_m_5_n_{m}.csv"
    df = os.path.join(main_dir,synthetic_data, filename_sds)
    sds.append(df)

    filename_sds = f"ctgan/sds_ctgan_data_sd2011_clean_small_epochs_600_m_5_n_{m}.csv"
    df = os.path.join(main_dir,synthetic_data, filename_sds)
    sds.append(df)

# key variables
key_var_5 = ["sex","edu","age","marital","region"]
key_var_4 = ["sex","edu","age","marital"]
key_var_3 = ["sex","edu","age"]

# target variables
target_var_1 = "income"
target_var_2 = "smoke"

# declare values
keys = [key_var_3,key_var_4,key_var_5]
target = [target_var_1,target_var_2]

df_output = []
count = 0

# calculate TCAP score
for s in sds:
    count = count + 1
    
    if count > 5:
        count = 1
    
    for t in target:
        for k in keys:
            num_keys = len(k)
            tcap_score = tcap(original=ods, synth=s, target=t, keys=k, num_keys=num_keys, verbose = False)
            
            if "synthpop" in s:
                sdg = "synthpop"
            elif "ctgan" in s:
                sdg = "ctgan"
            elif "datasynthesizer" in s:
                sdg = "datasynthesizer"
            else:
                value = None
    
            df_output.append([sdg, count, t, num_keys, round(tcap_score[0],3), round(tcap_score[1],3)])

# Create table
df_output_out = pd.DataFrame(df_output)
df_output_out.columns = ['sdg', 'copy', 'target', "number of keys", "tcap", "baseline"]    
df_output_out = df_output_out.sort_values(by=['sdg','number of keys','copy']).reset_index(drop=True)
print(df_output_out)

df_output_out = df_output_out.groupby(['sdg', 'target', "number of keys"]).mean().reset_index()
print(df_output_out)

# merge based on key values
data = {'number of keys': [3, 4, 5],
        'keys': [key_var_3,key_var_4,key_var_5]}
df = pd.DataFrame(data)

# df_output_out = pd.merge(df_output_out, df, on='number of keys', how='outer')
df_output_out = df_output_out.sort_values(by=['target', 'number of keys','sdg']).reset_index(drop=True)
print(df_output_out)
