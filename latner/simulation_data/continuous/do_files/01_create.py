'''
Top commands
'''

# load packages
import os
import pandas as pd
import numpy as np
import random

# file paths - adapt main_dir pathway
#main_dir = "N:/Ablagen/D01700-KEM/Latner/simulation/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/simulation_data/simulation_cont/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

os.chdir(main_dir)

# beginning commands
pd.options.display.max_columns = None
pd.options.display.width = 100
pd.set_option('display.float_format', '{:.2f}'.format)

# Set the random seed
np.random.seed(1234)
random.seed(1234)

'''
Create data
'''

# Set the number of observations
n = 1000

'''
Continuous data
income, age, wealth
'''

# Income: generate random numbers from a right-skewed distribution
income = np.random.lognormal(mean=10, sigma=1, size=n)
income = income.round(0)

# Age: generate random ages between 16 and 95
age = np.random.randint(16, 95, size=n)

'''
Wealth: generate random numbers with a right-skewed distribution with negative values at the bottom of the distribution
This approximately follows US wealth distribution according to the following report:
The wealth of households: 2020
Current Population Reports
By Donald Hays and Briana Sullivan
P70BR-181
August 2022
'''

wealth = np.random.lognormal(mean=11.9, sigma=1.75, size=n)
wealth = wealth.round(0)
wealth = wealth - 20000

# Create the DataFrame
df_cont = pd.DataFrame({'age': age, 'income': income, 'wealth': wealth})

'''
Save
'''

df_cont.to_csv(os.path.join(original_data,"ods_0.csv"), index=False)

print(df_cont.describe())
