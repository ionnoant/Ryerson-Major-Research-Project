"""
The purpose of this file is to test each of the models we plan on using 
in our project using default hyperparameters.

What are the algorithms we want to test?
1) Sarimax
2) Prophet
3) Lightgbm
4) XGBoost
5) Catboost
6) Time-series transformer

"""
# loading libraries
import numpy as np
import pandas as pd
import pickle 
import lightgbm
import catboost
import statsmodels
import xgboost as xgb
from fbprophet import Prophet
from sklearn.model_selection import TimeSeriesSplit, RandomizedSearchCV
from random import choice
import joblib

# helpful links to get us started.
# https://machinelearningmastery.com/sarima-for-time-series-forecasting-in-python/
# https://stackoverflow.com/questions/46732748/how-do-i-use-a-timeseriessplit-with-a-gridsearchcv-object-to-tune-a-model-in-sci
# https://machinelearningmastery.com/gradient-boosting-with-scikit-learn-xgboost-lightgbm-and-catboost/
# https://machinelearningmastery.com/use-features-lstm-networks-time-series-forecasting/
# https://neptune.ai/blog/lightgbm-parameters-guide

train_df = pd.read_pickle("Data/train_df_final_2020-06-27.pickle")

cv_params = ["month","dayofmonth","dayofweek","hour",
             "sqm","air_temperature","dew_temperature","wind_speed",
             "meter_reading"]

feature_params = ["month","dayofmonth","dayofweek","hour",
                  "air_temperature","dew_temperature","wind_speed"]

num_params = ["hour","sqm","air_temperature","dew_temperature","wind_speed"]

cat_params = ["month","dayofmonth","dayofweek"]

target_param = ["meter_reading"]

# creating a dataframe with a single building to test on.
single_building_df = train_df[train_df.building_id_kaggle == 138]



# we should scale the data for each building
# Hyperparameter grid
param_grid_lgbm = {
    'boosting_type': ['gbdt','goss','dart'],
   'min_data_in_leaf':list(range(25, 150,25)),
    'num_leaves': list(range(25, 150,25)),
    'learning_rate': [.001,.01,.05,.1,.2],
    'min_child_samples': list(range(25,150,25)),
    'reg_alpha': [0,0.1,0.5,0.9,1],
    'reg_lambda': [0,0.1,0.5,0.9,1],
    'colsample_bytree': list(np.linspace(0.6, 1, 5)),
    'subsample': list(np.linspace(0.5, 1, 5))
}

# creating time series cross validation object
tscv = TimeSeriesSplit()

# running randomized search time-series cross-validation
# on LGBM model
model_lgbm = lightgbm.LGBMRegressor()

# need to consider how much of the sample space we want to explore.
# currently only looking at less than 1%.
cv_lgbm = RandomizedSearchCV(model_lgbm,param_grid_lgbm,cv = tscv,scoring = 'neg_root_mean_squared_error',n_iter = 100)

search_lgbm = cv_lgbm.fit(single_building_df[feature_params], single_building_df[target_param])

# dump entire search object
joblib.dump(search_lgbm, 'Methodology and experiments output/lgbm models/gs_object.pickle')

search_lgbm = joblib.load('gs_object.pickle')

##########
#Catboost
##########

param_grid_cb = {'depth': list(range(3,10)),
                 'iterations': [100],
                  'learning_rate': [.001,.01,.05,.1,.2],
                 'l2_leaf_reg': list(range(1,10,2))}

model_cb = catboost.CatBoostRegressor()

cv_cb = RandomizedSearchCV(model_cb,param_grid_cb,cv = tscv,scoring = 'neg_root_mean_squared_error',n_iter = 100)

search_cb = cv_cb.fit(single_building_df[feature_params],
             single_building_df[target_param],
             cat_features = ["month","dayofmonth","dayofweek"],silent = True)

###############
# XGBoost
###############

param_grid_xgb = {'learning_rate': [.001,.01,.05,.1,.2],
                  'max_depth': list(range(3,10)),
                  'objective': ['reg:squarederror'],
                  'alpha': [0,0.1,0.5,0.7,1],
                  'lambda': [1,2,5,7,10]}

# need to convert all categorical data to one hot encoding with XGBoost

xgb_df = pd.concat( [single_building_df[num_params],
              pd.get_dummies(single_building_df[cat_params])],axis = 1)

model_xgb = xgb.XGBRegressor()

cv_xgb = RandomizedSearchCV(model_xgb,param_grid_xgb,cv = tscv,scoring = 'neg_root_mean_squared_error',n_iter = 100)

search_xgb = cv_xgb.fit(xgb_df,
             single_building_df[target_param])

#########
# Prophet 
#########

# prophet only requires a date-time and target column

prophet_df = single_building_df[["timestamp","meter_reading"]]

prophet_df.columns = ["ds","y"] # you need to do this or it won't work.

model_prophet = Prophet()

model_prophet.fit(prophet_df)


