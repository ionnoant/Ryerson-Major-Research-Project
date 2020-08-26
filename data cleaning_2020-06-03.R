# The purpose of this file is to clean all data within the MRP/Data project folder
# so that it is ready to be used for exploratory analysis.

# loading libraries 
library(tidyverse) # data analysis
library(scales) # date-time variable manipulation
library(imputeTS) # time series data imputation

# reading data into R
train_df <- read_csv("Data/electricity_cleaned.csv")
weather_df <- read_csv("Data/weather.csv")
building_df <- read_csv("Data/metadata.csv")

# custom ggplot theme
theme_ai <- function(){
  theme_minimal() +
    theme(
      text = element_text(color = "gray25"),
      plot.title = element_text(size=26),
      plot.subtitle = element_text(size = 24),
      axis.title = element_text(size = 16),
      axis.text = element_text(size=16),
      plot.caption = element_text(color = "gray30", size=12),
      plot.background = element_rect(fill = "white"),
      plot.margin = unit(c(5, 10, 5, 10), units = "mm"),
      axis.line = element_line(color="gray85"),
      axis.ticks.x = element_line(color="gray65"),
      panel.grid.major.y = element_line(colour = "gray80"),
      #panel.grid.major.y = element_blank(),
      panel.grid.minor.x =  element_blank(),
      panel.grid.major.x =  element_line(color="gray80"),
      legend.position = "none"
    )
  
}

##############################################
# cleaning hourly electricity data - train_df
##############################################

train_df <- train_df %>% gather(-timestamp,key = building_id, value = meter_reading)

train_df <-left_join(train_df,select(building_df,building_id,building_id_kaggle,site_id_kaggle))

train_df <- filter(train_df,!is.na(building_id_kaggle))

train_df <- filter(train_df,!is.na(meter_reading))

# creating new temporal variables.
train_df <- train_df %>%
  mutate(date = as.Date(timestamp),
         year = as.integer(format(timestamp,"%Y")),
         month = as.integer(format(timestamp,"%m")),
         dayofmonth = as.integer(format(timestamp,"%d")),
         dayofweek = as.integer(format(timestamp,"%u")),
         hour = as.integer(format(timestamp,"%H")))

# save an image of the current cleaning process
save.image("data cleaning_2020-06-04.RData")

# loading this image to continue analysis
load("data cleaning_2020-06-04.RData")

# For each building lets look at the number of zero 
# meter readings for each year, broken down by month and 
# and some summary statistics on the buildings meter
# reading data. 

total_number_of_meter_readings <-
  train_df %>%
  group_by(building_id,year) %>%
  summarise(total_count = n())

number_of_zero_meter_readings <- 
  train_df %>% 
  group_by(building_id, year) %>% 
  filter(meter_reading ==0 ) %>% 
  summarise(zero_count = n())

joined_meter_readings <-
  left_join(total_number_of_meter_readings,
            number_of_zero_meter_readings)

sum_count_frac_readings <- joined_meter_readings %>%
  mutate(frac_zero = zero_count/total_count) %>%
  group_by(building_id) %>%
  summarise(sum_total_count = sum(total_count,na.rm=T),
            sum_frac_zero = sum(frac_zero,na.rm=TRUE)) 

# This will the be metric we use to isolate viable 
# buildings for our analysis. Any builings that have a
# high overall count of meter readings with a low fraction
# of zero meter readings will be used. 

# How can I identify the marginal gain in sample size for 
# an increase in either the total count of meter readings
# OR the change in the fraction of the meter readings that
# is zero?

# What I could do is create a list that stores the length
# of the sum_count_frac_readings df for a change in either
# variable. 

sum_frac_zero_sample_size <- c()
sum_frac_zero_value <- c()

for (i in seq(0,1,.01)){
  length <-
    dim(sum_count_frac_readings %>%
          filter(sum_frac_zero<=i))[1]
  
  sum_frac_zero_sample_size <- 
    append(sum_frac_zero_sample_size,length)
  
  sum_frac_zero_value <-
    append(sum_frac_zero_value,i)
  
  if(i==1){
    sum_frac_zero_cumsum_df <-
      data.frame(frac_zero = sum_frac_zero_value,
                 sample_size = sum_frac_zero_sample_size)
  }
}

ggplot(sum_frac_zero_cumsum_df) +
  geom_line(aes(x = frac_zero, y = sample_size),size = 1)+
  geom_point(aes(x = frac_zero, y = sample_size), size = 2.5)+
  theme_ai()+
  scale_x_continuous(breaks = seq(0,1,.05))+
  scale_y_continuous(breaks = seq(0,1500,100),limits = c(500,1500))

# The figure provides a strong argument for having somewhere between 
# choosing a value between 1% and 5% since the increase in sample size
# is substantial.

# Assuming we choose the most conservative value of 1% how will our 
# sample size change if we now make a decision on the total number of 
# meter readings how large will our sample size be?

meter_reading_df <- data.frame(
  perc_sample = seq(0,1,.01),
  count_meter_readings = quantile(filter(sum_count_frac_readings, 
                                         sum_frac_zero <= 0.01)$sum_total_count,
                                  probs = seq(0,1,.01)))

# We only lose roughly 20% of our sample if we also require that 
# a building has at least 17,000 hourly meter readings, roughly
# 708 out of 730 days or 97% of the two years of 2016 and 2017 data.

# Since we know our data has AT most 1% zero values this means that 
# any building could have between 0 and 175 hours of meter readings with
# a value of zero. 

ggplot(meter_reading_df)+
  geom_line(aes(x = perc_sample, y = count_meter_readings),size = 1)+
  geom_point(aes(x = perc_sample, y = count_meter_readings), size = 2.5)+
  theme_ai()+
  scale_x_continuous(breaks = seq(0,1,.05))+
  scale_y_continuous(breaks = seq(0,18000,1000), limits = c(2500,18000))

# Filtering the train_df dataset so that it is restricted to building ids
# that have less than 1% zero values and 17,000 meter readings at a minimum.

filtered_building_ids <- 
  sum_count_frac_readings %>%
  filter(sum_total_count >= 17000,
         sum_frac_zero <=0.01) %>%
  select(building_id)

electricity_df <- filter(train_df, building_id %in% 
                           filtered_building_ids$building_id)


# for every profile in the data we are going through each image 
# and identifying which buildingload profiles are appropriate and 
# changing the interpolation method in some cases.

building_id_list <- sort(unique(electricity_df$building_id))

for(i in building_id_list){
  print(i)
  plt <- train_df %>%
    filter(building_id == i) %>%
    ggplot()+
    geom_point(aes(x = timestamp, y = meter_reading))+
    labs(x = "Month-year combination (MM-YY)",
         y = "Hourly consumption in kW",
         title = paste("Annual load profile for building id",i))+
    scale_x_datetime(labels = date_format("%m-%y"), date_breaks = "1 month") 
  
  primary_use_value <- building_df %>%
    filter(building_id == i ) %>%
    select(industry)
  
  primary_use_value <- str_replace(string = primary_use_value,pattern="/",replacement = "_")
  
  ggsave(plt, filename = paste("annual load profile_","building id_",i,"_",primary_use_value,".png",sep = ""),
         height = 3,
         width = 12)
}

save.image("data cleaning_2020-06-04.RData")


##############################################
# cleaning hourly weather data - weather_df
##############################################


# summarising data based on site_id
by(weather_df,weather_df$site_id,summary)

# Based on this summary its obvious that these sites are within 
# different temperature regions so it would not be appropriate to 
# impute missing values from one site to another.

# Most sites are missing quite a bit of data on cloud_coverage and
# precip_depth_1_hr so it would be difficult to impute these values
# for a specific site. 

# Moving forward we will only use air_temperature, dew_temperature,
# and wind_speed

# Lets produce annual plots of the weather data to see how it compares
# across site ids

site_id_list <- unique(weather_df$site_id)

weather_var_list <- colnames(weather_df)[c(3,5,10)]

for(i in site_id_list){
  
  print(i)
  
  for(j in weather_var_list){
    
    plt <- weather_df %>%
      filter(site_id == i) %>%
      ggplot()+
      geom_line(aes(x = timestamp, y = get(j)))+
      labs(x = "Month-year combination (MM-YY)",
           y = "Hourly weather in degrees celsius",
           title = paste("Annual", j," profile for site id",i))+
      scale_x_datetime(labels = date_format("%m-%y"), date_breaks = "1 month") 
    
    ggsave(plt, filename = paste("../MRP/Exploratory output/Annual profiles/","annual_", j,"_profile_",
                                 "site id_",i,".png",sep = ""),
           height = 3,
           width = 12)
    
  }
}

rm(plt)

# Now we are going to use the imputeTS package to produce some figures of missing 
# data
for(i in site_id_list){
  print(i)
  
  na_data <-  weather_df %>%
    filter(site_id == i)
  
  for(j in weather_var_list){
    print(j)
    sub_data <- as.numeric(select(na_data,j) %>% unlist())
    
    ggsave(plotNA.distribution(sub_data,
                               main = paste("Annual", j," NA (red) profile for site id",i),
                               xlab = "Hour",
                               ylab = "")
           , filename = paste("../MRP/Exploratory output/Missing data profiles/","annual", j," NA profile_",
                              "site id_",i,".png",sep = ""),
           height = 6,
           width = 10)
  }
}

save.image("data_cleaning_2020-06-04")

# once this is done delete na_data, site_id_list and weather_var_list

rm(na_data,site_id_list,weather_var_list)

# filtering air_temperature, dew_temperature, and wind_speed
# from weather_df

weather_df <- weather_df %>% 
  select(site_id,timestamp,airTemperature,
         dewTemperature,windSpeed)

names(weather_df) <- c("site_id","timestamp",
                       "air_temperature","dew_temperature",
                       "wind_speed")

# weather_df summary statistics pre-imputation
weather_df %>%
  group_by(site_id) %>%
  summarise(avg_air_temp = mean(air_temperature, na.rm = TRUE),
            sd_air_temp = sd(air_temperature, na.rm = TRUE),
            count_na_air_temp = sum(is.na(air_temperature)), 
            avg_dew_temp = mean(dew_temperature, na.rm = TRUE),
            sd_dew_temp = sd(dew_temperature, na.rm = TRUE),
            count_na_dew_temp = sum(is.na(dew_temperature)), 
            avg_wind_speed = mean(wind_speed, na.rm = TRUE),
            sd_wind_speed = sd(wind_speed, na.rm = TRUE),
            count_na_wind_speed = sum(is.na(wind_speed))) %>%
  write.csv(paste("pre imputation weather summary statistics_", Sys.Date(),".csv", sep=""))

# imputing values for each weather_df column using the na_kelman
# function in the imputeTS library - the na_kalman function is 
# recommended by the authors of the package as it typically 
# produces the best results.

imputed_weather_values <- as.data.frame(round(apply(weather_df[,c(3,4,5)], 2,na_kalman),2))

# replacing values in weather df with imputed values

weather_df$air_temperature <- imputed_weather_values$air_temperature

weather_df$dew_temperature <- imputed_weather_values$dew_temperature

weather_df$wind_speed <- imputed_weather_values$wind_speed

# weather_df summary statistics post-imputation

weather_df %>%
  group_by(site_id) %>%
  summarise(avg_air_temp = mean(air_temperature, na.rm = TRUE),
            sd_air_temp = sd(air_temperature, na.rm = TRUE),
            count_na_air_temp = sum(is.na(air_temperature)), 
            avg_dew_temp = mean(dew_temperature, na.rm = TRUE),
            sd_dew_temp = sd(dew_temperature, na.rm = TRUE),
            count_na_dew_temp = sum(is.na(dew_temperature)), 
            avg_wind_speed = mean(wind_speed, na.rm = TRUE),
            sd_wind_speed = sd(wind_speed, na.rm = TRUE),
            count_na_wind_speed = sum(is.na(wind_speed))) %>%
  write.csv(paste("post imputation weather summary statistics_", Sys.Date(),".csv", sep=""))

# stopped here
save.image("data_cleaning_2020-06-04")


############################################
# cleaning building meta data - building_df
############################################

# looking at missing values by site_id

by(data = building_df,INDICES = building_df$site_id,summary)

# lets restrict the dataset so it only contains the buildings from 
# our electricity df and so that we can calcualte statistics with it
# in our exploratory data file.

building_id_list <- unique(electricity_df$building_id)

building_df <- building_df %>%
  filter(building_id %in% building_id_list)

save.image("data_cleaning_2020-06-04")


# saving final electricity_df, weather_df, and building_df to 
# be used for all analysis in a new RData file.
save(electricity_df,weather_df,building_df,file = "clean data for analysis_2020-06-06.RData")
