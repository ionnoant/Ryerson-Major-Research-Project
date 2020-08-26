# -*- coding: utf-8 -*-
"""
Created on Mon Aug 10 21:20:00 2020

@author: Tony Joe
"""

import numpy as np
import pandas as pd
import pickle 
from sklearn.model_selection import TimeSeriesSplit, RandomizedSearchCV, GridSearchCV
import joblib
from sklearn.model_selection import ParameterGrid
from sklearn.metrics import mean_squared_error

# taken from https://stackoverflow.com/questions/47648133/mape-calculation-in-python
def percentage_error(actual, predicted):
    res = np.empty(actual.shape)
    for j in range(actual.shape[0]):
        if actual[j] != 0:
            res[j] = (actual[j] - predicted[j]) / actual[j]
        else:
            res[j] = predicted[j] / np.mean(actual)
    return res

def mean_absolute_percentage_error(y_true, y_pred): 
    return np.mean(np.abs(percentage_error(np.asarray(y_true), np.asarray(y_pred)))) * 100

def process_svr_data(df):
   
    scaler_svm = model_svr[0]
    
    target_svm = model_svr[1]
    
    df[num_params] = scaler_svm.transform(df[num_params])
    
    cat_data = pd.get_dummies(df[cat_params])
    
    df[target_param] = target_svm.transform(df[target_param])
    
    cat_data.reset_index(drop=True, inplace=True)
    
    df.reset_index(drop=True, inplace=True)
    
    df = pd.concat([df[target_param],df[num_params], cat_data], axis = 1)
    
    df = df.dropna()
    
    return df

def process_prophet_data(df):
    
    feature_params = ["timestamp","month","dayofmonth","dayofweek","hour",
                  "air_temperature","dew_temperature","wind_speed"]
    
    df = df[feature_params+target_param]

    df = df.dropna()
    
    return df.rename(columns={"timestamp": "ds"})

def process_cb_xgb_data(df):
        
    cat_data = pd.get_dummies(df[cat_params])
     
    cat_data.reset_index(drop=True, inplace=True)
    
    df.reset_index(drop=True, inplace=True)
     
    df = pd.concat([df[target_param],df[num_params], cat_data], axis = 1)
    
    df = df.dropna()
    
    return df
 
def process_lgbm_data(df):
    df = df[feature_params+target_param]
    df = df.dropna()
    return df

# useful string vectors to capture subsets of our training dataset below
cv_params = ["month","dayofmonth","dayofweek","hour",
             "sqm","air_temperature","dew_temperature","wind_speed",
             "meter_reading"]

feature_params = ["month","dayofmonth","dayofweek","hour",
                  "air_temperature","dew_temperature","wind_speed"]

num_params = ["hour","air_temperature","dew_temperature","wind_speed"]

cat_params = ["month","dayofmonth","dayofweek"]

target_param = ["meter_reading"]

# loading training dataset
train_df = pd.read_pickle("Data/train_df_final_2020-06-27.pickle")

test_df = pd.read_pickle("Data/test_df_final_2020-06-27.pickle")

# unique building ids based on building_id_kaggle feature
building_id = train_df.building_id_kaggle.unique()

i = building_id[1]

single_building_df  = train_df[train_df.building_id_kaggle == i]

single_building_df  = single_building_df[feature_params+target_param]

single_building_df = single_building_df.dropna()


from sklearn.svm import SVR
from sklearn.preprocessing import StandardScaler, MinMaxScaler
import lightgbm
import xgboost
import catboost
from sklearn.ensemble import StackingRegressor
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import OneHotEncoder
from sklearn.compose import make_column_transformer
from sklearn.preprocessing import StandardScaler, MinMaxScaler
from sklearn.impute import SimpleImputer
from mlxtend.feature_selection import ColumnSelector
count = 0
for i in building_id[507:]:
    
    single_building_df  = train_df[train_df.building_id_kaggle == i]
    single_building_df  = single_building_df[feature_params+target_param]
    single_building_df = single_building_df.dropna()

    print("building_id: ", i, "count: ", count)
    #creating SVR model 
    model_svr = joblib.load("Methodology and experiments output/svr models/svr_model_building_" + str(i) + ".pickle")
    model_svr_2 = SVR()
    svr_data = process_svr_data(single_building_df.copy())
    model_svr_2.fit(svr_data.iloc[:,1:],svr_data.iloc[:,0])
    joblib.dump(model_svr_2, "Methodology and experiments output/svr models_2020-08-10/svr_model_building_" + str(i) + ".pickle")
    
    # creating LGBM model
    model_lgbm = lightgbm.LGBMRegressor()
    lgbm_data = process_lgbm_data(single_building_df.copy())
    model_lgbm.fit(lgbm_data[feature_params],lgbm_data[target_param])
    joblib.dump(model_lgbm, "Methodology and experiments output/lgbm models_2020-08-10/lgbm_model_building_" + str(i) + ".pickle")
    
    # creating XGB model
    model_xgb = xgboost.XGBRegressor()
    cb_xgb_data = process_cb_xgb_data(single_building_df.copy())
    model_xgb.fit(cb_xgb_data.iloc[:,1:],cb_xgb_data[target_param])
    joblib.dump(model_xgb, "Methodology and experiments output/xgb models_2020-08-10/xgb_model_building_" + str(i) + ".pickle")
    
    # creating CB model
    model_cb = catboost.CatBoostRegressor()
    model_cb.fit(cb_xgb_data.iloc[:,1:],cb_xgb_data[target_param])
    joblib.dump(model_cb, "Methodology and experiments output/catboost models_2020-08-10/cb_model_building_" + str(i) + ".pickle")
    
    # creating stacked model
    categories = [
        single_building_df[column].unique() for column in cat_params
        ]
    # we need to make a pipeline for the svr model
    cat_pipeline = make_pipeline(
        SimpleImputer(missing_values=None, strategy='constant',
                      fill_value='missing'),
        OneHotEncoder(categories=categories))

    num_pipeline = make_pipeline(MinMaxScaler())

    svr_processor = make_column_transformer(
        (cat_pipeline, cat_params),
        (num_pipeline, num_params),
        remainder='passthrough')

    svr_pipeline = make_pipeline(svr_processor,
                                 model_svr_2)
    
    estimators = [
        ('lgbm',model_lgbm),
        ('svr', svr_pipeline)]
         #('xgb', xgb_pipeline),
         # ('cb', cb_pipeline)]

    model_stacked = StackingRegressor(
        estimators=estimators)

    model_stacked.fit(single_building_df[feature_params], np.array(single_building_df[target_param]))
    
    joblib.dump(model_stacked, "Methodology and experiments output/stacked models_2020-08-10/stacked_model_building_" + str(i) + ".pickle")
    
    count = count + 1
    
###############################################
# Testing a multi-layer perceptron architecture
###############################################
    
from sklearn.neural_network import MLPRegressor 
count = 0
for i in building_id:
    print("building_id: ", i, "count: ", count)
    single_building_df  = train_df[train_df.building_id_kaggle == i]
    single_building_df  = single_building_df[feature_params+target_param]
    single_building_df = single_building_df.dropna()
    
    model_mlp = MLPRegressor(learning_rate = 'adaptive', 
                         max_iter=500, 
                         batch_size=30, 
                         early_stopping= True,
                         hidden_layer_sizes=(50,2),
                         warm_start = True,random_state =1234)
    model_mlp.fit(single_building_df[feature_params],single_building_df[target_param])
    count = count + 1
    joblib.dump(model_mlp, "Methodology and experiments output/mlp models_2020-08-10/mlp_model_building_" + str(i) + ".pickle")

