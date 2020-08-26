# -*- coding: utf-8 -*-
"""
The purpose of this file is to setup our data for prediction.

"""
# Load libraries 
import numpy as np
import pandas as pd
import pickle 

electricity_df = pd.read_csv("Data/electricity_final_2020-06-24.csv")
weather_df = pd.read_csv("Data/weather_final_2020-06-24.csv")
building_df = pd.read_csv("Data/building_metadata_final_2020-06-24.csv")

# selecting columns for each dataframe
electricity_df = electricity_df.iloc[:,1:]
weather_df = weather_df.iloc[:,1:]
building_df = building_df.iloc[:,[0,1,2,3,4,5,7,10]]

# cleaning each data file 
electricity_df_2 = reduce_mem_usage(electricity_df)
weather_df = reduce_mem_usage(weather_df)
building_df = reduce_mem_usage(building_df)

# inserting old meter reading  
electricity_df_2['meter_reading'] = electricity_df['meter_reading']

# changing timestamp to date-time format
electricity_df_2.timestamp = pd.to_datetime(electricity_df_2.timestamp)
weather_df.timestamp = pd.to_datetime(weather_df.timestamp)

# need to join weather and building df and then joined dataframe
# with electricity df.

building_weather_df = \
building_df.merge(weather_df,
                    how = 'left',
                    left_on=['site_id'],
                    right_on =['site_id'])

electricity_building_weather_df = \
electricity_df_2.merge(building_weather_df,
                    how = 'left',
                    left_on=['building_id_kaggle',
                             'building_id',
                             'site_id_kaggle','timestamp'],
                    right_on =['building_id_kaggle',
                                'building_id',
                             'site_id_kaggle',
                             'timestamp'])

# saving individual files as pickle format (better? Yes, absolutely)
electricity_df.to_pickle("electricity_final_2020-06-24.pickle")
weather_df.to_pickle("weather_final_2020-06.24.pickle")
building_df.to_pickle("building_metadata_final_2020-06-24.pickle")

electricity_building_weather_df= \
electricity_building_weather_df.iloc[:,[0,1,2,3,4,5,6,7,8,9,
                                        11,12,13,14,15,16,17]]

electricity_building_weather_df.to_pickle("electricity_building_weather_final_2020-06-26.pickle")


# ok lets reset this and load the combined dataframe from a pickle file.
electricity_building_weather_df = pd.read_pickle("Data/electricity_building_weather_final_2020-06-26.pickle")

# lets separate the data into a training and test set
train_df = electricity_building_weather_df[electricity_building_weather_df.year == 2016]

test_df = electricity_building_weather_df[electricity_building_weather_df.year == 2017]

# saving the train and test file as a pickle file for easy loading.
train_df.to_pickle("train_df_final_2020-06-26.pickle")

test_df.to_pickle("test_df_final_2020-06-26.pickle")

# 2020-06-27 I need to reload the data and format all celandar data to
# categorical, crap. 

train_df = pd.read_pickle("Data/train_df_final_2020-06-26.pickle")

train_df[['month',"year","dayofmonth","dayofweek"]] = \
    train_df[['month',"year","dayofmonth","dayofweek"]].astype('category')

train_df.to_pickle("Data/train_df_final_2020-06-27.pickle")

test_df = pd.read_pickle("Data/test_df_final_2020-06-26.pickle")

test_df[['month',"year","dayofmonth","dayofweek"]] = \
    test_df[['month',"year","dayofmonth","dayofweek"]].astype('category')

test_df.to_pickle("Data/test_df_final_2020-06-27.pickle")