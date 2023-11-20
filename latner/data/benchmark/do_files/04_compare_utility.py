'''
TOP COMMANDS
# https://docs.sdv.dev/sdv/single-table-data/evaluation
'''

# load libraries
import os
import pandas as pd

from sdv.metadata import SingleTableMetadata

from sdv.evaluation.single_table import evaluate_quality
from sdv.evaluation.single_table import run_diagnostic

# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/simulation_data/benchmark/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"

os.chdir(main_dir)

# beginning commands
pd.set_option('display.width', 1000)
pd.set_option('display.float_format', '{:.2f}'.format)

# Set the random seed
my_seed = 1233

'''
LOAD DATA
'''

data = ["grid","gridr", # Simulated data
        "adult","sd2011_small", "sd_2011"] # Real data
type = ["ctgan","datasynthesizer","synthpop"] # Real data

data = ["sd2011_small"] # Real data
type = ["datasynthesizer"] # Real data

copies = [5]

for d in data:

    # Create a unique filename based on the values
    filename_ods = f"{d}.csv"
    filename_path = os.path.join(original_data, filename_ods)
    df_ods = pd.read_csv(filename_path, index_col=False)

    for t in type:     
        
        df_sds = pd.DataFrame() # empty data frame

        for m in copies:
        
            for j in range(m):    
                
                j = j + 1

                # Create a unique filename based on the values
                filename_sds = f"sds_{t}_{d}_m_{m}_n_{j}.csv"
                folder_sds = f"{t}/"
                filename_path = os.path.join(synthetic_data, folder_sds, filename_sds)
                sds = pd.read_csv(filename_path, index_col=False)
                df_sds = pd.concat([df_sds, sds], ignore_index=True)

    metadata = SingleTableMetadata()
    metadata.detect_from_dataframe(data=df_ods)
    
    # evaluate quality
    quality_report = evaluate_quality(
        real_data=df_ods,
        synthetic_data=df_sds,
        metadata=metadata)
    
    # run diagnostic
    diagnostic_report = run_diagnostic(
        real_data=df_ods,
        synthetic_data=df_sds,
        metadata=metadata)

quality_report.get_score()
quality_report.get_properties()
quality_report.get_details(property_name='Column Shapes')

diagnostic_report.get_results()
diagnostic_report.get_properties()
diagnostic_report.get_details(property_name='Coverage')

