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

df_ods.head(5)
df_ods.describe(include='all')
print(df_ods['edu'].value_counts(normalize=True,dropna=False))

ods = os.path.join(main_dir,original_data, "sd2011_clean_small.csv")
synth = os.path.join(main_dir,synthetic_data, "datasynthesizer/sds_datasynthesizer_sd2011_clean_small_k_2_e_0_m_5_n_1.csv")
synth = os.path.join(main_dir,synthetic_data, "synthpop/sds_synthpop_sd2011_clean_small_m_5_n_1.csv")

keys = ["sex","edu","age","marital","region"]
target = "income"
num_keys = len(keys)

output = tcap(original=ods, synth=synth, target=target, keys=keys, num_keys=num_keys, verbose = False)
output[1]


# read in the data
orig = pd.read_csv(ods)
syn = pd.read_csv(synth)

# define the keys and target. using the num_keys parameter means that a dataset with any number of columns can
# be used, and only the relevant keys analysed
keys_target = keys.copy()
keys_target.append(target)

# select just the required columns (keys and target)
orig = orig[keys_target]
syn = syn[keys_target]

# count the categories for the target (for calculating baseline)
uvd = orig[target].value_counts()

# use groupby to get the equivalance classes for synthetic data
eqkt_syn = pd.DataFrame({'count' : syn.groupby( keys_target ).size()}).reset_index()   # with target
eqkt_syn.shape

eqk_syn = pd.DataFrame({'count' : syn.groupby( keys_target[:num_keys] ).size()}).reset_index() # without target
eqk_syn.shape

# equivalance classes for original data without target
eqk_orig = pd.DataFrame({'count' : orig.groupby( keys_target[:num_keys] ).size()}).reset_index()
eqk_orig

# merge with original to calculate baseline
orig_merge_eqk = pd.merge(orig, eqk_orig, on= keys_target[:num_keys]) 
orig_merge_eqk.rename({'count': 'count_eqk_orig'}, axis=1, inplace=True)
orig_merge_eqk.head()
orig_merge_eqk

# calculate the baseline
uvt = sum(uvd[orig_merge_eqk[target]]/sum(uvd))
baseline = uvt/len(orig)

# calculate synthetic cap score. merge syn eq classes (with keys) with syn eq classes (with keys/target)
syn_merge = eqk_syn.merge(eqkt_syn, on=keys_target[:num_keys])
syn_merge

syn_merge.head()

syn_merge['prop'] = syn_merge['count_y']/syn_merge['count_x']
syn_merge['prop'].describe()
syn_merge['prop'].value_counts()

syn_merge.head()

syn_merge.head(20)

# filter out those less than tau=1
syn_merge = syn_merge[syn_merge['prop'] >= 1]
syn_merge.shape

pd.set_option('display.max_rows', None)  # Display all rows
print(syn_merge)

# merge with original, if in syn eq classes (just keys) then this is a matching record (Taub)
#syn_merge = syn_merge.merge(orig_merge_eqk, on=keys_target[:num_keys], how='inner')

test = syn_merge.merge(orig, on=keys_target[:num_keys], how='inner')
test.shape


syn_merge = syn_merge.merge(orig, on=keys_target[:num_keys], how='inner')

matching_records = len(syn_merge)
matching_records

# drop records where the targets are not equal
syn_merge = syn_merge[syn_merge[target + '_x']==syn_merge[target + '_y']]
dcaptotal = len(syn_merge)
dcaptotal

if matching_records == 0:
    tcap_undef = 0
else:
    tcap_undef = dcaptotal/matching_records
  
# output is [the TCAP as used by Taub, and the baseline]. Modify as required
output = ([tcap_undef,baseline])

output
