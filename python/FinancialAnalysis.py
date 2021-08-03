'''

This code is a mess - it currently doesn't have console support; I just do things down below. I will clean this up in the future, and improve documentation.


Written by Tommy Guess


'''



import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import math
from time import time
from datetime import datetime


def clean(df):
    '''
    In: dataframe
    Out: dataframe

    This function cleans the dataframe using the headings below.
    I run this function ahead of time and make a new CSV, since it take a long time to execute.
    
    '''
    

    # Calculate trip duration
    ## - using timestamps, this creates a new column with the time of the trip in seconds
    durationList = []
    for loc in range(len(df.index)):
        #adapted from https://stackoverflow.com/questions/1345827/how-do-i-find-the-time-difference-between-two-datetime-objects-in-python

        #convert strings into datetime objects
        dateStart = datetime.strptime(df.iloc[loc]['tpep_pickup_datetime'], '%Y-%m-%d  %H:%M:%S')
        dateEnd = datetime.strptime(df.iloc[loc]['tpep_dropoff_datetime'], '%Y-%m-%d  %H:%M:%S')
        duration = dateEnd - dateStart

        durationList.append(duration.total_seconds()) 
        
        if len(durationList)%10000 == 0:
            print(len(durationList))
        
    df.insert(3,"trip_time",durationList) #add list of lengths to the dataframe as a new column


    rowCount = len(df.index)
    print("Total number of rows: ",rowCount)

    # Remove rows with outliers/impossible numbers. Prints percentage of rows removed.
    ## remove zero or less passengers
    passChangeNum = df.loc[df.passenger_count <=0, 'passenger_count'].count()
    print("Zero or fewer passenger_count: {}, {}% of dataset.".format(passChangeNum, (100*(passChangeNum/rowCount))),)
    df = df[df['passenger_count'] != 0] 

    ## remove negative tips
    tipChangeNum = df.loc[df.tip_amount <0, 'tip_amount'].count() 
    print("Negative tip_amount: {}, {}% of dataset".format(tipChangeNum, (100*(tipChangeNum/rowCount))))
    df = df[df['tip_amount'] >= 0] #remove tips below zero

    ## remove zero or negative trip distance
    distanceChangeNum = df.loc[df.trip_distance <=0, 'trip_distance'].count()
    print("Zero or less trip_distance: {}, {}% of dataset".format(distanceChangeNum, (100*(distanceChangeNum/rowCount))))
    df = df[df['trip_distance'] >= 0] #remove trip distances below zero

    ## remove zero or negative total cost
    amountChangeNum = df.loc[df.total_amount <=0, 'total_amount'].count()
    print("Zero or less total_amount: {}, {}% of dataset".format(amountChangeNum, (100*(amountChangeNum/rowCount))))
    df = df[df['total_amount'] >= 0] #remove trips with zero or less total cost

    #convert Y N to 0 1
    df['store_and_fwd_flag'] = df['store_and_fwd_flag'].map({'Y':1,'N':0}) #https://stackoverflow.com/questions/40901770/is-there-a-simple-way-to-change-a-column-of-yes-no-to-1-0-in-a-pandas-dataframe


    ## Uncomment this to drop datetime columns.
    #df = df.drop(columns = ['tpep_pickup_datetime','tpep_dropoff_datetime']) #no longer need exact time for this analysis
    return df


    #datetimeobj = datetime.strptime(str, '%m/%d/%Y  %I:%M:%S %p')


def zscore_normalization(pd_attribute):
    '''
    From one of the homework problems, currently unused
    '''

    avg = mean(pd_attribute)

    sd = std(pd_attribute)

    zList = []

    for row in pd_attribute:
        zList.append((float(row)-avg)/sd)

    return zList
   

#get correlation matrix
def correlMatrix(DF,attributeList=['VendorID', 'trip_time','passenger_count','trip_distance','RatecodeID','payment_type','fare_amount',
                 'extra','mta_tax','tip_amount','tolls_amount',
                 'improvement_surcharge', 'total_amount','congestion_surcharge']):

    '''
    In: dataframe, optional attribute list to use in matrix
    Out: dataframe with correlation score between each row/column
    
    '''
    
    corrDF = pd.DataFrame(index = attributeList, columns = attributeList) #dataframe to store calculated correlations

    for attName1 in attributeList:
        for attName2 in attributeList:
            if math.isnan(corrDF[attName1][attName2]): #only do calculation if it hasn't been done

                print("Correlation between {} and {}: ".format(attName1,attName2),end='')

                pd_att1 = DF[attName1]
                pd_att2 = DF[attName2]

                cor = pd_att1.corr(pd_att2)
                print(cor)

                corrDF[attName1][attName2] = cor #put result in both halves of diagonal
                corrDF[attName2][attName1] = cor


    '''
    # for exporting straight to file.

    exportName = 'correlationMatrix.csv'
    corrDF.to_csv(exportName)
    print("Exported file as: {}".format(exportName))
    '''

    return corrDF

def pickup_dropoff_matrix(DF):
    '''
    Calculates a matrix with the total number of trips for pickup/dropoff - a simple count. This is not currently designed for arbitrary attributes, and I will extend it.
    '''
    
    numberList = range(1,266) # max number of location IDs

    locDF = pd.DataFrame(index = numberList, columns = numberList) # the matrix we will eventually return
    locDF = locDF.fillna(0) # fill with zeros


    count = 0
    for  index, row in DF.iterrows(): # iterate through each combination. This code is not optimized - I may fix it depending on the time.
    
        pick = row['PULocationID']
        drop = row['DOLocationID']
        
        currentVal = locDF.at[pick,drop]
        currentVal += 1
        locDF.at[pick,drop] = currentVal

        count += 1
        if (count%10000 == 0):
            print(count)

    return locDF

        
def pickupTip(DF):
    '''
    Calculates basic stats for the tips by pickup ID.
    This (and the next function) will be replaced by a more powerful function in the near future. 
    
    '''
    
    numberList = range(1,266)
    cols = ['minTip','meanTip','maxTip','medianTip']
    tipDF = pd.DataFrame(index = numberList, columns = cols)

    for i in range(1,266):
        rows = DF.loc[DF['PULocationID'] == i]

        tipAtt = rows['tip_amount']

        tipDF.at[i,'minTip'] = tipAtt.min()
        tipDF.at[i,'meanTip'] = tipAtt.mean()
        tipDF.at[i,'maxTip'] = tipAtt.max()
        tipDF.at[i,'medianTip'] = tipAtt.median()
            
    return tipDF


def dropoffTip(DF):
    '''
    See previous function.
    
    '''
    numberList = range(1,266)
    cols = ['minTip','meanTip','maxTip','medianTip']
    tipDF = pd.DataFrame(index = numberList, columns = cols)

    for i in range(1,266):
        rows = DF.loc[DF['DOLocationID'] == i]

        tipAtt = rows['tip_amount']

        tipDF.at[i,'minTip'] = tipAtt.min()
        tipDF.at[i,'meanTip'] = tipAtt.mean()
        tipDF.at[i,'maxTip'] = tipAtt.max()
        tipDF.at[i,'medianTip'] = tipAtt.median()
            
    return tipDF



#############################
# end of definitions
#############################

#note: all of this can be removed; it contains whatever I was working on last.




timeStart = time()
#filename = "short.csv"
#filename = "yellow_tripdata_2020-02.csv"

filename = "cleaned2.csv"
'''
attributeList = ['passenger_count','trip_distance','RatecodeID','payment_type','fare_amount',
                 'extra','mta_tax','tip_amount','tolls_amount',
                 'improvement_surcharge', 'total_amount','congestion_surcharge']

'''

attributeList = ['VendorID', 'trip_time','passenger_count','trip_distance','RatecodeID','payment_type','fare_amount',
                 'extra','mta_tax','tip_amount','tolls_amount',
                 'improvement_surcharge', 'total_amount','congestion_surcharge']

DF = pd.read_csv(filename)

#correlMatrix(DF,attributeList)
#locDF = pickup_dropoff_matrix(DF)
#pickupTipMtx = pickupTip(DF)

dropoffTipMtx = dropoffTip(DF)

timeEnd = time()
print("total time: {}".format(timeEnd-timeStart))





