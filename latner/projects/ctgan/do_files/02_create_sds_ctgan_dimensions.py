'''
TOP COMMANDS
'''

# load libraries
import os
import pandas as pd
import time

from sdv.single_table import CTGANSynthesizer
from sdv.metadata import SingleTableMetadata


# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/ctgan/"
data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
duration = "duration/"

os.chdir(main_dir)

'''
Vary dimensions
'''

data = ["sd2011_clean_small"] # Real data

embedding = [16,64,128]

diminsions_1_1 = [128]
diminsions_1_2 = [128,128]
diminsions_1_3 = [128,128,128]

diminsions_2_1 = [256]
diminsions_2_2 = [256,256]
diminsions_2_3 = [256,256,256]

diminsions_3_1 = [512]
diminsions_3_2 = [512,512]
diminsions_3_3 = [512,512,512]

dimensions = [diminsions_1_1,diminsions_1_2,diminsions_1_3,
              diminsions_2_1,diminsions_2_2,diminsions_2_3,
              diminsions_3_1,diminsions_3_2,diminsions_3_3
              ]

for df in data:
    df_duration = [] # empty data frame
    for e in embedding:
        for d in dimensions:
            # Create a unique filename based on the values
            filename_ods = f"{df}.csv"
            df_ods = pd.read_csv(os.path.join(original_data, filename_ods))
            
            # synthesize data
            metadata = SingleTableMetadata()
            metadata.detect_from_dataframe(data=df_ods)
                
            # Step 1: Create the synthesizer
            synthesizer = CTGANSynthesizer(metadata, 
                                           embedding_dim = e, 
                                           generator_dim = d,
                                           discriminator_dim = d,
                                           verbose=True)
            
            # Start the timer
            time_start = time.time()
        
            # Step 2: Train the synthesizer
            synthesizer.fit(df_ods)
                                                
            # Stop the timer
            time_end = time.time()
                
            # Calculate the execution time for this iteration
            time_duration = time_end - time_start
                
            # Store duration values
            df_duration.append(["ctgan",df,e,d,time_duration])
       
            # Step 3: Generate synthetic data set (sds)
            sds = synthesizer.sample(num_rows=len(df_ods.index))
        
            # Create a unique filename based on the values
            filename_sds = f"sds_ctgan_data_{df}_embedding_{e}_dimensions_{d}.csv"
                            
            # Step 4: Save synthetic data set (sds)
            sds.to_csv(os.path.join(synthetic_data, filename_sds), index=False)
        
            print("")
            print("data: ",df)
            print("embedding: ",e)
            print("dimensions: ",d)

    # save duration by data set
    df_duration_out = pd.DataFrame(df_duration)
    df_duration_out.columns = ['type', 'data', 'embedding', 'dimensions', "duration"]    
    filename_duration = f"duration_ctgan_data_{df}_dimensionality.csv"
    df_duration_out.to_csv(os.path.join(duration, filename_duration), index=False)

