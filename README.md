# BENCHMARKING MACHINE LEARNING PREDICTION METHODS ON AN OPEN DATASET OF HOURLY ELECTRICITY DATA FOR A SET OF NON-RESIDENTIAL BUILDINGS

**Anthony Ionno** 

**Master of Science 2021**

**Data Science and Analytics**

**Ryerson University** 

**ABSTRACT**

Roughly 64.7% of Canada’s annual electricity consumption was attributed to non-residential (commercial and industrial) buildings in 2016 [1]. Smart meters provide companies and building managers with an opportunity to track electricity consumption at an hourly or sub-hourly granularity. Efficient management of a non-residential building’s electricity consumption is beneficial for the building manager in terms of bill savings, the electricity system, the demand reduction in peak hours or demand shifting can defer or even prevent significant system infrastructure investment [2], and for the environment in the form of reduced carbon emissions.

The aim of this paper is to present a variety of supervised and unsupervised machine learning methods that might allow companies or building managers to better predict future electricity consumption to make more informed decisions on building operations. 

In this paper we train six machine learning models on one year’s worth of hourly electricity data for each of the 828 non-residential buildings in our dataset. Randomised search time-series cross-validation was used to determine optimal hyperparameters for each building and model combination. 

We also present a cluster analysis model as an exploratory technique to understand how daily electricity load profiles can be grouped and compared more generally in a variety of circumstances. 

Our test results show that across our sample and each of the models tested Mean Absolute Percentage Error (MAPE) varied considerably and that this is likely due to significant differences in a building’s electricity consumption patterns between the training and test set. We also found that Gradient Boosting Decision Trees (GBDT) outperformed all the other machine learning models we tested by a significant margin.

**Keywords**
machine learning benchmarking; building energy prediction; energy forecasting; cluster analysis; smart meters; artificial neural networks; support vector machines; gradient boosting decision trees; stacked generalisation
