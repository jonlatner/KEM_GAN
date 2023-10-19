'''
TOP COMMANDS
'''

from ucimlrepo import fetch_ucirepo 

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

# fetch dataset 
adult = fetch_ucirepo(id=2) 
  
# data (as pandas dataframes) 
X = adult.data.features 
y = adult.data.targets 
  
# metadata 
print(adult.metadata) 
  
# variable information 
print(adult.variables) 

# Create a unique filename based on the values
filename = f"ods_rows_{r}_cols_{c}_vals_{v}.csv"
    
df_ods.to_csv(os.path.join(original_data, filename), index=False)
