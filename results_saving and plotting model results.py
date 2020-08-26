# -*- coding: utf-8 -*-
"""
The purpose of this file is to load a set of machine learning models
for each non-residential building and run that model on our test set.

We will collect each model's hyperparameters and its test set RMSE and
MAPE error rate and store this inforamtion into a list. 

Once this information has been collected we will plot error rate results.
"""
# loading libraries
import numpy as np
import pandas as pd
import pickle 
import joblib
from sklearn.metrics import mean_squared_error, mean_absolute_error
import seaborn as sns
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import OneHotEncoder
from sklearn.compose import make_column_transformer
from sklearn.preprocessing import StandardScaler, MinMaxScaler
from sklearn.impute import SimpleImputer
from mlxtend.feature_selection import ColumnSelector


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

# loading separate models for a non-resdiential building

feature_params = ["month","dayofmonth","dayofweek","hour",
                  "air_temperature","dew_temperature","wind_speed"]

num_params = ["hour","air_temperature","dew_temperature","wind_speed"]

cat_params = ["month","dayofmonth","dayofweek"]

target_param = ["meter_reading"]

test_df = pd.read_pickle("Data/test_df_final_2020-06-27.pickle")

# unique building ids based on building_id_kaggle feature
building_id = test_df.building_id_kaggle.unique()

results = list()

count = 1

for i in building_id[507:]:
    
    print("building id: ", i, " count: ", count)

    single_building_df  = test_df[test_df.building_id_kaggle == i]


    model_svr = joblib.load("Methodology and experiments output/svr models/svr_model_building_" + str(i) + ".pickle")
    
    model_svr_2 = joblib.load("Methodology and experiments output/svr models_2020-08-10/svr_model_building_" + str(i) + ".pickle")

    model_lgbm = joblib.load("Methodology and experiments output/lgbm models_2020-08-10/lgbm_model_building_" + str(i) + ".pickle")
  
    model_xgb = joblib.load("Methodology and experiments output/xgb models_2020-08-10/xgb_model_building_" + str(i) + ".pickle")
  
    model_cb = joblib.load("Methodology and experiments output/catboost models_2020-08-10/cb_model_building_" + str(i) + ".pickle")
    
    model_stacked = joblib.load("Methodology and experiments output/stacked models_2020-08-10/stacked_model_building_" + str(i) + ".pickle")

    svr_data = process_svr_data(single_building_df.copy())

    lgbm_data = process_lgbm_data(single_building_df.copy())

    prophet_data = process_prophet_data(single_building_df.copy())

    cb_xgb_data = process_cb_xgb_data(single_building_df.copy())

    single_building_df  = single_building_df[feature_params+target_param]

    single_building_df = single_building_df.dropna()

# svr

    rmse_svr = mean_squared_error(model_svr[1].inverse_transform(np.reshape(model_svr_2.predict(svr_data.iloc[:,1:]),(-1,1))),\
                                  single_building_df[target_param],squared = False)

    mape_svr = mean_absolute_percentage_error(model_svr[1].inverse_transform(np.reshape(model_svr_2.predict(svr_data.iloc[:,1:]),(-1,1))),\
                                              single_building_df[target_param])

#lgbm
    rmse_lgbm = mean_squared_error(model_lgbm.predict(lgbm_data[feature_params]),\
                                   single_building_df[target_param],squared = False)

    mape_lgbm = mean_absolute_percentage_error(model_lgbm.predict(lgbm_data[feature_params]),\
                                               lgbm_data[target_param])
 
#xgb
    rmse_xgb = mean_squared_error(model_xgb.predict(cb_xgb_data.iloc[:,1:]),\
                                  cb_xgb_data[target_param], squared = False)   

    mape_xgb = mean_absolute_percentage_error(model_xgb.predict(cb_xgb_data.iloc[:,1:]),\
                                              cb_xgb_data[target_param])   
    
#catboost
    rmse_cb = mean_squared_error(model_cb.predict(cb_xgb_data.iloc[:,1:]),\
                                 cb_xgb_data[target_param], squared = False)   

    mape_cb = mean_absolute_percentage_error(model_cb.predict(cb_xgb_data.iloc[:,1:]),\
                   cb_xgb_data[target_param])    
# stacked
    rmse_stacked = mean_squared_error(model_stacked.predict(lgbm_data[feature_params]),\
                   np.array(lgbm_data[target_param]), squared = False)   

    mape_stacked = mean_absolute_percentage_error(model_stacked.predict(lgbm_data[feature_params]),\
                   np.array(lgbm_data[target_param])) 
    
    
    temp_results = [i,rmse_svr,mape_svr,rmse_lgbm,mape_lgbm,rmse_xgb,mape_xgb,rmse_cb,mape_cb,rmse_stacked,mape_stacked]

    results.append(temp_results)
    
    count = count + 1
    
df = pd.DataFrame(results)
df.columns =['building_id_kaggle','rmse_svr','mape_svr','rmse_lgbm',\
             'mape_lgbm','rmse_xgb','mape_xgb','rmse_cb',\
             'mape_cb','rmse_stacked','mape_stacked']

df.to_pickle("Data/final_testset_results_2020-08-11.pickle")

df.to_csv("Data/final_testset_results_2020-08-11.csv")


# loading and saving results for mlp
results = list()

count = 1

for i in building_id:
     
    print("building id: ", i, " count: ", count)
    single_building_df  = test_df[test_df.building_id_kaggle == i]
    single_building_df  = single_building_df[feature_params+target_param]
    single_building_df = single_building_df.dropna()
    model_mlp = joblib.load("Methodology and experiments output/mlp models_2020-08-10/mlp_model_building_" + str(i) + ".pickle")
    rmse_mlp = mean_squared_error(model_mlp.predict(single_building_df[feature_params]),\
                   np.array(single_building_df[target_param]), squared = False)   

    mape_mlp = mean_absolute_percentage_error(model_mlp.predict(single_building_df[feature_params]),\
                   np.array(single_building_df[target_param]))
        
    temp_results = [i,rmse_mlp,mape_mlp]
    
    results.append(temp_results)
    count = count + 1
df = pd.DataFrame(results)
df.columns =['building_id_kaggle','rmse_mlp','mape_mlp']
df.to_pickle("Data/final_mlp_results_2020-08-11.pickle")
df.to_csv("Data/final_mlp_results_2020-08-11.csv")