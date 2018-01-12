import pandas as pd

def safe_div(x,y):
    if y == 0:
        return y
    return x / y

# to be used in apply function
# cost method is already passed    
def applyCalculateSpend (df_name, cost_method_col, metric_col, rate_col, total_planned_col):
    calculations = {
            'CPMV'  : df_name[metric_col] / 1000 * df_name[rate_col],
            'Free'  : 0
            }
    df_method = df_name[cost_method_col]
    return calculations.get(df_method, "not in dict")

'''
# USED FOR TESTS
test_df = pd.read_csv('data_for_hg/prisma_test.csv')
    
# Use the where method
test_df['Spend'] = np.where(test_df['cost method'] == 'CPMV', test_df.metric/1000 * test_df.rate,
           np.where(test_df['cost method'] == 'Flat', 5,0))
'''

# Open all tables
dcm_hg = pd.read_csv('data_for_hg/dcm_homegoods_Sep-Oct.csv', header=10)
dv_hg = pd.read_csv('data_for_hg/dv_homegoods.csv')
prisma_hg = pd.read_excel('data_for_hg/prisma_homegood.xlsx', sheetname='2h')
prisma_hg = pd.concat([prisma_hg, pd.read_excel('data_for_hg/prisma_homegood.xlsx', sheetname='q4')])

# DCM
dcm_hg['PCode'] = dcm_hg['Package/Roadblock'].str[:6] # get PCode
dcm_hg['Date'] = pd.to_datetime(dcm_hg['Date']) # get date format
dcm_hg['Week'] = dcm_hg['Date'].dt.week # get week
dcm_hg = dcm_hg.iloc[:,3:] #Drop the left three cols

dcm_hg = dcm_hg.groupby(['PCode', 'Week'])['Impressions', 'Clicks'].sum() #.reset_index() # group by pcode & week

# DV
dv_hg['PCode'] = dv_hg['Placement Name'].str[:6] # get pcode
dv_hg = dv_hg[['Media Property', 'PCode','Date', 'GroupM Billable Impressions']] # select relevant cols
dv_hg['Date'] = pd.to_datetime(dv_hg['Date']) # set date to a datetime format
dv_hg['Week'] = dv_hg['Date'].dt.week # get the week number from the datetime format
dv_hg = dv_hg.groupby(['PCode','Week'])['GroupM Billable Impressions'].sum() #.reset_index() # Becomes series
dv_hg = pd.DataFrame(dv_hg)

# Prisma
prisma_hg = prisma_hg[['Campaign flight start date',
              'Campaign flight end date',
              'Flight start date',
              'Flight end date',
              'Supplier',
              'Placement number',
              'Cost method',
              'Rate',
              'Total Planned Cost']] # get relevant items from the date time format 
prisma_hg['prisma_flight_length_hg'] = abs(prisma_hg['Flight end date'] - prisma_hg['Flight start date'])
prisma_hg = prisma_hg.fillna(0) # fill nans as 0s

# Full Outer Join dv and dcm
dcm_dv_hg = pd.merge(dcm_hg, dv_hg, left_index=True, right_index=True, how='outer').reset_index().fillna(0)

# loop to GO combine dcm_dv_hg and prisma_hg based on the weeks in the former dataset.
# Only keep relevant pcodes' data that are present in prisma_cleaned data.
# Then calculate based on "cost method" column

# variable declaration
dig_spend = None

for i in dcm_dv_hg['Week'].unique():
    weekly_dcm_dv_hg = dcm_dv_hg.loc[dcm_dv_hg['Week'] == i]

    #Indenting for testing. Unindent and remove if statement
#    if i == 40:
    # left join prisma and dcm_dv_loop item "weekly_dcm_dv_hg" on the PCode
    data = pd.merge(prisma_hg, weekly_dcm_dv_hg, left_on='Placement number', right_on='PCode', how='left')
    data['spend'] = data.apply(lambda row: applyCalculateSpend(
        row,
        cost_method_col='Cost method',
        metric_col='GroupM Billable Impressions',
        rate_col='Rate',
        total_planned_col='Total Planned Cost'), axis = 1)
    data = data.groupby(['Supplier']).agg({'spend': 'sum', 'Week': 'min'}).reset_index()
    # Unionize
    dig_spend = pd.concat([dig_spend, data])
        
dig_spend.to_csv('dig_spend.csv', index=False)




print('\n'*3)
print("""

MINDSHARE
MARKETING SCIENCES                          
                                                                        
""")
print('Total spend is %f\n' % (dig_spend.sum(numeric_only=True)['spend']))
print('Total spend by partner is...\n')
print(dig_spend.groupby(dig_spend.Supplier).sum().spend)

    
    
    
    
    
    
    
    
    