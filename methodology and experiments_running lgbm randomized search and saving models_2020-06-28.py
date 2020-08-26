# -*- coding: utf-8 -*-
"""
The purpose of this file is to run a randomized time-series cross-validation 
search across the defined light gbm hyper-parameter space below for each 
building in the dataset. 
"""
# loading libraries
import numpy as np
import pandas as pd
import pickle 
from sklearn.model_selection import TimeSeriesSplit, RandomizedSearchCV, GridSearchCV
import joblib
from sklearn.model_selection import ParameterGrid
from sklearn.metrics import mean_squared_error

# useful string vectors to capture subsets of our training dataset below
cv_params = ["month","dayofmonth","dayofweek","hour",
             "sqm","air_temperature","dew_temperature","wind_speed",
             "meter_reading"]

feature_params = ["month","dayofmonth","dayofweek","hour",
                  "air_temperature","dew_temperature","wind_speed"]

num_params = ["hour","sqm","air_temperature","dew_temperature","wind_speed"]

cat_params = ["month","dayofmonth","dayofweek"]

target_param = ["meter_reading"]

# loading training dataset
train_df = pd.read_pickle("Data/train_df_final_2020-06-27.pickle")

# unique building ids based on building_id_kaggle feature
building_id = train_df.building_id_kaggle.unique()

# creating time series cross validation object
tscv = TimeSeriesSplit()
    
###########################
# Support Vector Regression
###########################
    
from sklearn.svm import SVR
from sklearn.preprocessing import StandardScaler, MinMaxScaler

param_grid_svm ={'kernel': ['rbf'], 'gamma': [.0001,.001, .01,.1],
                     'C': [1, 10,20,50,100,150]}



for i in building_id:
    print(i)
    single_building_df = train_df[train_df.building_id_kaggle == i]
    
    # for each building we need to scale all quantitative variables using
    # standard scaler and categorical variables into one-hot encodings.
    
    num_params = ["hour","air_temperature","dew_temperature","wind_speed"]
    
    scaler_svm = MinMaxScaler()
    
    target_scaler_svm = MinMaxScaler()
    
    scaler_svm.fit(single_building_df[num_params])
    
    target_scaler_svm.fit(single_building_df[target_param])
    
    single_building_df[num_params] = scaler_svm.transform(single_building_df[num_params])
    
    cat_data = pd.get_dummies(single_building_df[cat_params])
    
    single_building_df[target_param] = target_scaler_svm.transform(single_building_df[target_param])
    
    cat_data.reset_index(drop=True, inplace=True)
    
    single_building_df.reset_index(drop=True, inplace=True)
    
    building_df = pd.concat([single_building_df[target_param],single_building_df[num_params], cat_data], axis = 1)
    
    building_df = building_df.dropna()
    
    model_svm = SVR()
    cv_svm = GridSearchCV(model_svm,param_grid_svm,
                             cv = tscv,scoring = 'neg_root_mean_squared_error')
    search_svm = cv_svm.fit(building_df.loc[:,building_df.columns != "meter_reading"],np.array(building_df['meter_reading']))
    joblib.dump([scaler_svm,target_scaler_svm, search_svm], "Methodology and experiments output/svr models/svr_model_building_" + str(i) + ".pickle")

############
# Light GBM
############

import lightgbm

# parameter grid for hyper-parameter tuning
param_grid_lgbm = {
    'boosting_type': ['dart'],
   'min_data_in_leaf':list(range(25, 150,25)),
    'num_leaves': list(range(25, 150,25)),
    'learning_rate': [.001,.01,.05,.1,.2],
    'min_child_samples': list(range(25,150,25)),
    'reg_alpha': [0,0.1,0.5,0.9,1],
    'reg_lambda': [0,0.1,0.5,0.9,1],
    'colsample_bytree': list(np.linspace(0.6, 1, 5)),
    'subsample': list(np.linspace(0.5, 1, 5))
}



# looping through each building and 
for i in building_id[711:]:
    print(i)
    single_building_df = train_df[train_df.building_id_kaggle == i]
    # running randomized search time-series cross-validation
    # on LGBM model
    model_lgbm = lightgbm.LGBMRegressor()
    cv_lgbm = RandomizedSearchCV(model_lgbm,param_grid_lgbm,
                             cv = tscv,scoring = 'neg_root_mean_squared_error',
                             n_iter = 500)
    search_lgbm = cv_lgbm.fit(single_building_df[feature_params], single_building_df[target_param])
    joblib.dump(search_lgbm, "Methodology and experiments output/lgbm models/lgbm_model_building_" + str(i) + ".pickle")
    
    
############
# Catboost
############

import catboost

param_grid_cb = {'depth': list(range(3,10)),
                 'iterations': [100],
                  'learning_rate': [.001,.01,.05,.1,.2],
                 'l2_leaf_reg': list(range(1,10,2)),
                 'bagging_temperature': list(range(0,10))}   
    
for i in building_id[507]:
    print(i)
    single_building_df = train_df[train_df.building_id_kaggle == 691]
    model_cb = catboost.CatBoostRegressor()

    cv_cb = RandomizedSearchCV(model_cb,param_grid_cb,cv = tscv,scoring = 'neg_root_mean_squared_error',n_iter = 100)

    search_cb = cv_cb.fit(single_building_df[feature_params],
             single_building_df[target_param],
             cat_features = ["month","dayofmonth","dayofweek"],silent = True)
    
    joblib.dump(search_cb, "Methodology and experiments output/catboost models/cb_model_building_" + str(i) + ".pickle")
    
############
# XGBoost
############

import xgboost

param_grid_xgb  = {'booster': ['gbtree'],
                 'verbosity': [0],
                  'learning_rate': [.001,.01,.05,.1,.2],
                 'max_depth': list(range(3,10)),
                 'lambda': list(range(1,10,2)),
                 'alpha': list(range(1,10,2)),
                 'n_estimators': [100,150,200]}  


for i in building_id[370:402]:
    print(i)
    single_building_df = train_df[train_df.building_id_kaggle == i]
    
    cat_data = pd.get_dummies(single_building_df[cat_params])
        
    cat_data.reset_index(drop=True, inplace=True)
    
    single_building_df.reset_index(drop=True, inplace=True)
    
    building_df = pd.concat([single_building_df[num_params], cat_data], axis = 1) 
    
    model_xgb = xgboost.XGBRegressor()
    
    cv_xgb = RandomizedSearchCV(model_xgb,param_grid_xgb,cv = tscv,scoring = 'neg_root_mean_squared_error',n_iter = 100)
    
    search_xgb = cv_xgb.fit(building_df,
             single_building_df[target_param])
    
    joblib.dump(search_xgb, "Methodology and experiments output/xgb models/xgb_model_building_" + str(i) + ".pickle")
    
# need to redo 369 to +35 for catboost. FUCK.  
    
    
############
# Prophet
############

from fbprophet import Prophet


param_grid_prophet = {'seasonality_mode':('multiplicative','additive'),
               'changepoint_prior_scale':[0.1,0.5],
              'holidays_prior_scale':[0.1,0.5],
              'n_changepoints' : [100,200]}

grid = ParameterGrid(param_grid_prophet)

site_id_location = pd.read_csv("Data/site id locations.csv")


# redefining the number of splits here because prophet is slow.

# creating time series cross validation object
tscv = TimeSeriesSplit(n_splits = 2)

# testing stuff here
for i in building_id:
    print("building_id: ", i)
    single_building_df = train_df[train_df.building_id_kaggle == i]
    
    site_id_arr = np.unique(single_building_df.site_id_kaggle)
    
    if(site_id_arr not in [8,14]):
       
       site_id_location_arr = site_id_location[site_id_location.site_id_kaggle == np.int(site_id_arr)].location
       
       site_id_location_arr.astype(str)
    
    
# number of combinations in grid.
    num_combos = np.arange(1,16) 
    
# randomly shuffling values to choose from.
    np.random.shuffle(num_combos)
    
    num_combos = num_combos[:5]
    
    prophet_df = single_building_df[['timestamp','meter_reading']]
    
    prophet_df = prophet_df.rename(columns={"timestamp": "ds", "meter_reading": "y"})
        
    cv_mean_squared_error = list()
    
    building_rmse_grid_result_list = pd.DataFrame(columns = ['RMSE','Parameters'])
    
    count = 0
    
    for j in num_combos:
        print('num combo: ', j)
        for train, test in tscv.split(prophet_df):
            model_prophet = Prophet(changepoint_prior_scale = grid[j].get("changepoint_prior_scale"),
                                    holidays_prior_scale = grid[j].get('holidays_prior_scale'),
                                    n_changepoints = grid[j].get('n_changepoints'),
                                    seasonality_mode = grid[j].get('seasonality_mode'),
                                    weekly_seasonality=True,
                                    daily_seasonality = True,
                                    yearly_seasonality = False
                                    )
        
            if(site_id_arr not in [8,14]):
                model_prophet.add_country_holidays(country_name= site_id_location_arr[0])
            
            model_prophet.fit(prophet_df.iloc[train,:])
        
            future = pd.DataFrame(prophet_df.iloc[test,0])
        
            forecast = model_prophet.predict(future)
        
            print(mean_squared_error(prophet_df.iloc[test,1], forecast[['yhat']], squared = False))
        
            cv_mean_squared_error.append(mean_squared_error(prophet_df.iloc[test,1], forecast[['yhat']], squared = False))
            
            count = count + 1
                
        building_rmse_grid_result_list = building_rmse_grid_result_list.append({'RMSE':np.mean(cv_mean_squared_error),\
                                                                                 'Parameters':grid[j]},ignore_index=True)
            
        print("mean cv error: " ,np.mean(cv_mean_squared_error))

    building_rmse_grid_result_list = building_rmse_grid_result_list.sort_values(by=['RMSE'])
    
    building_rmse_grid_result_list = building_rmse_grid_result_list.reset_index(drop=True)
    
    final_values = dict(building_rmse_grid_result_list['Parameters'][0])
    
    model_prophet = Prophet(changepoint_prior_scale =  final_values.get("changepoint_prior_scale"),
                         holidays_prior_scale =  final_values.get('holidays_prior_scale'),
                         n_changepoints =  final_values.get('n_changepoints'),
                         seasonality_mode =  final_values.get('seasonality_mode'),
                         weekly_seasonality=True,
                         daily_seasonality = True,
                         yearly_seasonality = False
                               )
    if(site_id_arr not in [8,14]):
        model_prophet.add_country_holidays(country_name= site_id_location_arr[0])
                
    model_prophet.fit(prophet_df)
       
    joblib.dump(model_prophet, "Methodology and experiments output/prophet models/prophet_model_building_" + str(i) + ".pickle")  
    
# Based on the initial results of the first 44 buildings it does not seem worth the effort to run a time series cross-validation
# strategy using Prophet. For the remaining 800 buildings I am simply going to run prophet with default values. 
    
for i in building_id[168:]:
    print("building_id: ", i)
    single_building_df = train_df[train_df.building_id_kaggle == i]
    
    site_id_arr = np.unique(single_building_df.site_id_kaggle)
    
    if(site_id_arr not in [8,14]):
       
       site_id_location_arr = site_id_location[site_id_location.site_id_kaggle == np.int(site_id_arr)].location
       
       site_id_location_arr.astype(str)
       
    model_prophet = Prophet(weekly_seasonality=True,
                         daily_seasonality = True,
                         yearly_seasonality = False
                         )
    
    if(site_id_arr not in [8,14]):
         model_prophet.add_country_holidays(country_name= np.array(site_id_location_arr)[0])
         
    prophet_df = single_building_df[['timestamp','meter_reading']]
    
    prophet_df = prophet_df.rename(columns={"timestamp": "ds", "meter_reading": "y"})
                
    model_prophet.fit(prophet_df)
       
    joblib.dump(model_prophet, "Methodology and experiments output/prophet models/prophet_model_building_" + str(i) + ".pickle")  
    

###############    
# Stacked model
###############
    
from sklearn.ensemble import StackingRegressor
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import OneHotEncoder
from sklearn.compose import make_column_transformer
from sklearn.preprocessing import StandardScaler, MinMaxScaler
from sklearn.impute import SimpleImputer
from mlxtend.feature_selection import ColumnSelector

count = 782
num_params = ["hour","air_temperature","dew_temperature","wind_speed"]

for i in building_id[782:]:
    
    print('building id: ', i, ' count: ', count)
    model_svr = joblib.load("Methodology and experiments output/svr models/svr_model_building_" + str(i) + ".pickle")

    model_lgbm = joblib.load("Methodology and experiments output/lgbm models/lgbm_model_building_" + str(i) + ".pickle")
    
    model_xgb = joblib.load("Methodology and experiments output/xgb models/xgb_model_building_" + str(i) + ".pickle")
    
    model_cb = joblib.load("Methodology and experiments output/catboost models/cb_model_building_" + str(i) + ".pickle")

    single_building_df = train_df[train_df.building_id_kaggle == i]
    single_building_df = single_building_df[feature_params+target_param]
    single_building_df = single_building_df.dropna()
   

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
                                 model_svr[2].best_estimator_)
    
    xgb_processor = make_column_transformer(
        (cat_pipeline, cat_params),
         (num_pipeline, num_params),
        remainder = 'passthrough')
    
    xgb_pipeline = make_pipeline(xgb_processor,
                                 model_xgb.best_estimator_)
    
    cb_pipeline = make_pipeline(xgb_processor,
                                 model_cb.best_estimator_)
    

    estimators = [
        ('lgbm',model_lgbm.best_estimator_),
        ('svr', svr_pipeline)]
         #('xgb', xgb_pipeline),
         # ('cb', cb_pipeline)]

    model_stacked = StackingRegressor(
        estimators=estimators)

    model_stacked.fit(single_building_df[feature_params], np.array(single_building_df[target_param]))

    mean_squared_error(model_stacked.predict(single_building_df[feature_params]),np.array(single_building_df[target_param]),squared = False)

    joblib.dump(model_stacked, "Methodology and experiments output/stacked model/stacked_model_building_" + str(i) + ".pickle")  
    
    count = count + 1
  
