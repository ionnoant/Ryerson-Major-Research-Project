#' The purpose of this script is to read in the lgbm MAPE test set results
#' and compare hourly building electricity data for building's with low MAPE
#' scores against buildings with high MAPE scores. We choose a value of MAPE
#' score below 5% as low based on our literature review. 


# Reading libraries
library(tidyverse)
library(magrittr)
library(lubridate)
library(scales)

# # Reading data into R
# mape_test_scores <- 
#   read_csv("Data/mape_lgbm_combined_data_2020-08-12.csv")
# 
# train_df <- 
#   read_csv("Data/train_df_final_2020-06-27.csv") %>%
#   select(timestamp:wind_speed)
# 
# test_df <- 
#   read_csv("Data/test_df_final_2020-06-27.csv") %>%
#   select(timestamp:wind_speed)
# 
# full_df <- 
#   rbind(train_df,test_df)
# 
# save(full_df,mape_test_scores,file = "Data/comparing buildings based on MAPE test score.RData")
# custom ggplot theme
theme_ai <- function(){
  theme_minimal() +
    theme(
      text = element_text(color = "black"),
      plot.title = element_text(size=26),
      plot.subtitle = element_text(size = 24),
      axis.title = element_text(size = 20),
      axis.text = element_text(size=18),
      plot.caption = element_text(color = "black", size=12),
      plot.background = element_rect(fill = "white"),
      plot.margin = unit(c(5, 10, 5, 10), units = "mm"),
      axis.line = element_line(color="gray85"),
      axis.ticks.x = element_line(color="gray65"),
      panel.grid.major.y = element_line(colour = "gray80"),
      #panel.grid.major.y = element_blank(),
      panel.grid.minor.x =  element_blank(),
      panel.grid.major.x =  element_line(color="gray80"),
      legend.position = "bottom",
      legend.title = element_text(size=14),
      legend.text =  element_text(size=12)
    )
  
}


load("Data/comparing buildings based on MAPE test score.RData")

perc_25_value <- 
  quantile(mape_test_scores$metric_value, probs = 0.25)

perc_75_value <- 
  quantile(mape_test_scores$metric_value, probs = 0.75)

low_mape_building_ids <- 
  filter(mape_test_scores,metric_value <perc_25_value)$building_id_kaggle

high_mape_building_ids <- 
  filter(mape_test_scores,metric_value >perc_75_value)$building_id_kaggle




for(i in c(low_mape_building_ids)){
  print(i)
  plt <- full_df %>%
    filter(building_id_kaggle == i) %>%
    ggplot()+
    geom_point(aes(x = timestamp, y = meter_reading, color = as.factor(year)),
               alpha = 0.9)+
    geom_smooth(aes(x = timestamp, y = meter_reading), color = "gray25", size = 2,
                se = FALSE)+
    labs(x = "Month-year combination (MM-YY)",
         y = "Hourly consumption in kW",
         #title = paste("Annual load profile for building id",i),
         color = "Year")+
    scale_x_datetime(labels = date_format("%m-%y"), date_breaks = "1 month")+
    theme_ai()+ 
    theme(axis.text.y = element_text(colour = "black"),
    axis.text.x = element_text(colour = "black"),
    legend.text = element_text(size = 20, color = "black"),
    legend.title = element_text(size = 24, color = "black"),
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(0.5,"cm") )+
    guides(col = guide_legend(ncol = 8))
  
  
  ggsave(plt, filename = paste("annual load profile_","building id_",i,".png",sep = ""),
         height = 7,
         width = 22)
}

# point 1 - buildings with a lower mape score typically have less inter-hour electricity
# consumption variation.

full_df <- 
  full_df %>%
  group_by(building_id_kaggle,year) %>%
  mutate(meter_reading_perc_year = meter_reading/sum(meter_reading))



test <- 
  full_df %>%
  mutate(low_mape = ifelse(building_id_kaggle %in% low_mape_building_ids,1,0)) %>%
  group_by(building_id_kaggle,low_mape,year,hour) %>%
  summarise(std_perc_meter_reading = sd(meter_reading_perc_year))

plt_pt_1 <- test %>%
  ggplot()+
  geom_boxplot(aes( x = as.factor(hour), 
                    y = log(std_perc_meter_reading), 
                    fill = as.factor(low_mape)))+
  labs(x = "Hour", y = "Logged Average Standard Deviation of Electricity Consumption",
       fill = "Low MAPE score (1 = Yes)")+
  theme_ai()

ggsave(plt_pt_1, file = "results_point 1 figure.png",height = 10, width = 15)
  
# point 2 - low MAPE scorers are more highly correlated with the features in our 
# dataset

test <- 
  full_df %>%
  mutate(low_mape = ifelse(building_id_kaggle %in% low_mape_building_ids,1,0)) %>%
  group_by(building_id_kaggle,low_mape,year) %>%
  summarise(corr_perc_meter_reading = cor(meter_reading,air_temperature,use = "complete.obs"))

test %>%
  ggplot()+
  geom_boxplot(aes( x = low_mape, 
                    y = corr_perc_meter_reading, 
                    fill = as.factor(low_mape)))+
  labs(x = "Hour", y = "Logged Average Standard Deviation of Electricity Consumption",
       fill = "Low MAPE score (1 = Yes)")+
  theme_ai()
  

