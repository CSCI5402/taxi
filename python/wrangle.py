import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# feb_20 = pd.read_csv("../data/yellow_tripdata_2020-02.csv", header=0)
# mar_20 = pd.read_csv("../data/yellow_tripdata_2020-03.csv", header=0)
apr_20 = pd.read_csv("../data/yellow_tripdata_2020-04.csv", header=0)

apr_20[['pickup_date', 'pickup_time']] = apr_20.tpep_pickup_datetime.str.split(expand=True)
apr_20[['dropoff_date', 'dropoff_time']] = apr_20.tpep_dropoff_datetime.str.split(expand=True)

del apr_20['pickup_date']
del apr_20['dropoff_date']
del apr_20['store_and_fwd_flag']
del apr_20['tpep_pickup_datetime']
del apr_20['tpep_dropoff_datetime']

apr_20['pickup_time'] = pd.to_timedelta(apr_20['pickup_time']).astype('timedelta64[s]').astype(float)
apr_20['dropoff_time'] = pd.to_timedelta(apr_20['dropoff_time']).astype('timedelta64[s]').astype(float)


