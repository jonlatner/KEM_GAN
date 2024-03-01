#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 29 09:47:29 2024

@author: jonathanlatner
"""


'''
LOAD DATA
'''

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

data = ["sd2011_clean_small"] # Real data
epochs = [100,300,600,900]
discriminator = [1,5,10]
generator = [1,5,10]
batch = [500]
frequency = [True,False]
copies = [1]

generator_dim
discriminator_dim

for d in discriminator:
    for e in epochs:
        for b in batch:
            for f in frequency:
                for m in copies: 

                    time.sleep(5)

                    for j in range(m):    

                        j = j + 1

                        # Step 1: Create the synthesizer
                        synthesizer = CTGANSynthesizer(metadata, 
                                                       log_frequency = f,
                                                       discriminator_steps=d,
                                                       epochs=e,
                                                       batch_size=b,
                                                       verbose=True)
                        
                       
                        # Step 2: Train the synthesizer
                        synthesizer.fit(df_ods)
                    
                        # Step 3: Generate synthetic data set (sds)
                        sds = synthesizer.sample(num_rows=len(df_ods.index))
                        
                    
                        # Create a unique filename based on the values
                        filename = f"sds_ctgan_tuning_e_{e}_d_{d}_b_{b}_f_{f}_m_{m}_n_{j}.csv"

                        # Step 4: Save synthetic data set (sds)
                        sds.to_csv(os.path.join(synthetic_data, filename), index=False)
    
                        print("discriminator:",d)
                        print("epochs:",e)
                        print("batch:",b)
                        print("frequency:",f)
                        print("copies:",m)
                        print("number:",j)
