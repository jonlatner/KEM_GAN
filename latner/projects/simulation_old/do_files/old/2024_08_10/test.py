import pandas as pd

def roc_univariate(original, synthetic, var_num):
    # create frequency tables for the original and synthetic data, on the variable
    orig_table = pd.crosstab(index=original.iloc[:, var_num], columns='count').reset_index()
    syn_table = pd.crosstab(index=synthetic.iloc[:, var_num], columns='count').reset_index()
    
    # calculate the proportions by dividing by the number of records in each dataset
    orig_table['prop'] = orig_table['count'] / len(original)
    syn_table['prop'] = syn_table['count'] / len(synthetic)
    
    # merge the two tables, by the variable
    combined = pd.merge(orig_table, syn_table, on=orig_table.columns[0], how='outer', suffixes=('.orig', '.syn'))
    
    # merging will induce NAs where there is a category mismatch - i.e. the category exists in one dataset but not the other
    # to deal with this set the NA values to zero:
    combined = combined.fillna(0)
    
    # get the maximum proportion for each category level:
    combined['max'] = combined[['prop.orig', 'prop.syn']].max(axis=1)
    # get the minimum proportion for each category level:
    combined['min'] = combined[['prop.orig', 'prop.syn']].min(axis=1)
    
    # roc is min divided by max (a zero value for min results in a zero for ROC, as expected)
    combined['roc'] = combined['min'] / combined['max']
    combined.loc[combined['roc'].isna(), 'roc'] = 1
    
    return combined['roc'].mean()

# Test the function with the simulation data
# Simulation data for testing
original = pd.DataFrame({'var1': [1, 2, 1, 3, 2, 1, 2, 2, 3, 1]})
synthetic = pd.DataFrame({'var1': [1, 1, 2, 2, 2, 3, 3, 3, 3, 1]})

roc_value = roc_univariate(original, synthetic, 0)
roc_value
