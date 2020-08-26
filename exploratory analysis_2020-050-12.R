# The purpose of this file is to perform exploratory analysis on the
# clean data final_2020_05-12.RData file in the MRP project folder

# What this script will do is produce either graphs or numbers that
# identify whether we can observe some sort-of relationship between
# our variable of interest, electricity consumption/demand, and the
# feature variables in the dataset. 

# loading libraries 
library(tidyverse) # data analysis
library(NbClust) # k-means metrics
library(ggmap) # geospatial analysis
library(revgeo) # reverse geocoding
library(lubridate) #date-time functions
library(ClusterR) # k-means++ algo
library(patchwork) # merging ggplots
library(openair) # calendar plot
library(Rtsne) # t-sne dimension reduction algo


# load data
load("clean data for analysis_2020-06-06.RData")

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
      #legend.position = "top",
      legend.title = element_text(size=14),
      legend.text =  element_text(size=12)
    )
  
}

# Function to produce summary statistics (mean and +/- sd)
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

###################
# energy statistics
###################

#############################
#  monthly consumption 
#############################

monthly_kwh <- 
  electricity_df %>%
  group_by(building_id,year,month) %>%
  summarise(monthly_kwh = sum(meter_reading)) %>%
  left_join(select(building_df,building_id,primaryspaceusage,sub_primaryspaceusage))

primary_use_type <- unique(monthly_kwh$primaryspaceusage)

sub_primary_use_type <- unique(monthly_kwh$sub_primaryspaceusage)

primary_use_type_avg_monthly_kwh<- 
  monthly_kwh %>%
  group_by(primaryspaceusage, year) %>%
  summarise(perc_1 = quantile(monthly_kwh, prob = .01),
            perc_10 = quantile(monthly_kwh, prob = .10),
            perc_25 = quantile(monthly_kwh, prob = .25),
            perc_50 = quantile(monthly_kwh, prob = .50),
            perc_75 = quantile(monthly_kwh, prob = .75),
            perc_90 = quantile(monthly_kwh, prob = .90),
            perc_99 = quantile(monthly_kwh, prob = .99)) 

primary_use_type_avg_monthly_kwh %>%
  write.csv("primary use median monthly consumption by year_2020-06-07.csv")

sub_use_type_med_monthly_kwh <- 
  monthly_kwh %>%
  group_by(sub_primaryspaceusage, year) %>%
  summarise(perc_1 = quantile(monthly_kwh, prob = .01),
            perc_10 = quantile(monthly_kwh, prob = .10),
            perc_25 = quantile(monthly_kwh, prob = .25),
            perc_50 = quantile(monthly_kwh, prob = .50),
            perc_75 = quantile(monthly_kwh, prob = .75),
            perc_90 = quantile(monthly_kwh, prob = .90),
            perc_99 = quantile(monthly_kwh, prob = .99))

sub_use_type_med_monthly_kwh %>%
  write.csv("sub primary use median monthly consumption by year_2020-06-07.csv")


es_plt_1 <- 
  monthly_kwh %>%
  group_by(building_id,year,primaryspaceusage) %>%
  summarise(mean_monthly_kwh = mean(monthly_kwh)) %>%
  ggplot()+
  geom_boxplot(aes(y = reorder(primaryspaceusage,desc(primaryspaceusage)), 
               x = mean_monthly_kwh/1000,
               fill = as.factor(year)))+
  #geom_label(aes(y = reorder(primaryspaceusage,perc_50), 
  #               x = perc_50,
  #               label = round(perc_50), group = as.factor(year)),
  #           position = position_dodge(0.9),
  #           hjust = 1.5)+
  theme_ai() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))+
  labs(x = "Average monthly consumption in Megawatt hour (MWh)",
       y = "Primary use type",
       fill = "Year")+
  scale_x_continuous(breaks = seq(0,2500,200))

ggsave(es_plt_1, filename = "Average monthly consumption in mwh by primary use type.png",
       height = 5.5, 
       width = 15)

# producing graphs for sub primary use types is not worth it because there
# are so many sub use types - a graph shows nothing really. 

test <- 
  monthly_kwh %>%
  group_by(building_id,year,primaryspaceusage) %>%
  summarise(mean_monthly_kwh = mean(monthly_kwh))

quantile(test$mean_monthly_kwh, probs =seq(0,1,.1))

#############################
# median peak demand
#############################

monthly_peak_kw <- 
  electricity_df %>%
  group_by(building_id,year,month) %>%
  summarise(monthly_peak_kw = max(meter_reading)) %>%
  left_join(select(building_df,building_id,primaryspaceusage,sub_primaryspaceusage))

primary_use_type <- unique(monthly_peak_kw$primaryspaceusage)

sub_primary_use_type <- unique(monthly_peak_kw$sub_primaryspaceusage)

primary_use_type_med_monthly_peak_kw <- 
  monthly_peak_kw  %>%
  group_by(primaryspaceusage, year) %>%
  summarise(perc_1 = quantile(monthly_peak_kw, prob = .01),
            perc_10 = quantile(monthly_peak_kw, prob = .10),
            perc_25 = quantile(monthly_peak_kw, prob = .25),
            perc_50 = quantile(monthly_peak_kw, prob = .50),
            perc_75 = quantile(monthly_peak_kw, prob = .75),
            perc_90 = quantile(monthly_peak_kw, prob = .90),
            perc_99 = quantile(monthly_peak_kw, prob = .99)) 

primary_use_type_med_monthly_peak_kw   %>%
  write.csv("primary use median monthly peak kw by year_2020-06-07.csv")

sub_use_type_med_monthly_peak_kw <- 
  monthly_peak_kw  %>%
  group_by(sub_primaryspaceusage, year) %>%
  summarise(perc_1 = quantile(monthly_peak_kw, prob = .01),
            perc_10 = quantile(monthly_peak_kw, prob = .10),
            perc_25 = quantile(monthly_peak_kw, prob = .25),
            perc_50 = quantile(monthly_peak_kw, prob = .50),
            perc_75 = quantile(monthly_peak_kw, prob = .75),
            perc_90 = quantile(monthly_peak_kw, prob = .90),
            perc_99 = quantile(monthly_peak_kw, prob = .99))

sub_use_type_med_monthly_peak_kw %>%
  write.csv("sub primary use median monthly peak kw by year_2020-06-07.csv")


es_plt_2 <- 
  monthly_peak_kw %>%
  group_by(building_id,year,primaryspaceusage) %>%
  summarise(mean_peak_demand = mean(monthly_peak_kw)) %>%
  ggplot()+
  geom_boxplot(aes(y = reorder(primaryspaceusage,desc(primaryspaceusage)), 
               x = mean_peak_demand, 
               fill = as.factor(year)))+
  #geom_label(aes(y = reorder(primaryspaceusage,perc_50), 
  #               x = perc_50,
  #               label = round(perc_50), group = as.factor(year)),
  #           position = position_dodge(0.9),
  #           hjust = 1.5)+
  theme_ai() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))+
  labs(x = "Average monthly peak demand in Megawatts (MW)",
       y = "Primary use type",
       fill = "Year")+
  scale_x_continuous(breaks = seq(0,2000,200), limits = c(0,2000))

ggsave(es_plt_2, filename = "average monthly peak demand in mw by primary use type.png",
       height = 5.5, 
       width = 15) 

#save.image("exploratory analysis_2020-06-06")

#############################
# load factor
#############################

annual_load_factor <- 
  electricity_df %>%
  group_by(building_id,year) %>%
  summarise(annual_peak_kw = max(meter_reading),
            annual_kwh = sum(meter_reading),
            meter_reading_count = n()) %>%
  mutate(annual_load_factor = annual_kwh/(annual_peak_kw * meter_reading_count)) %>%
  left_join(select(building_df,building_id,primaryspaceusage,sub_primaryspaceusage))

primary_use_type <- unique(annual_load_factor$primaryspaceusage)

sub_primary_use_type <- unique(annual_load_factor$sub_primaryspaceusage)

primary_use_type_med_annual_load_factor <- 
  annual_load_factor  %>%
  group_by(primaryspaceusage, year) %>%
  summarise(perc_1 = quantile(annual_load_factor, prob = .01),
            perc_10 = quantile(annual_load_factor, prob = .10),
            perc_25 = quantile(annual_load_factor, prob = .25),
            perc_50 = quantile(annual_load_factor, prob = .50),
            perc_75 = quantile(annual_load_factor, prob = .75),
            perc_90 = quantile(annual_load_factor, prob = .90),
            perc_99 = quantile(annual_load_factor, prob = .99)) 

primary_use_type_med_annual_load_factor   %>%
  write.csv("primary use median annual load factor by year_2020-06-07.csv")

sub_use_type_med_annual_load_factor <- 
  annual_load_factor  %>%
  group_by(sub_primaryspaceusage, year) %>%
  summarise(perc_1 = quantile(annual_load_factor, prob = .01),
            perc_10 = quantile(annual_load_factor, prob = .10),
            perc_25 = quantile(annual_load_factor, prob = .25),
            perc_50 = quantile(annual_load_factor, prob = .50),
            perc_75 = quantile(annual_load_factor, prob = .75),
            perc_90 = quantile(annual_load_factor, prob = .90),
            perc_99 = quantile(annual_load_factor, prob = .99))

sub_use_type_med_annual_load_factor %>%
  write.csv("sub primary use median annual load factor by year_2020-06-07.csv")

es_plt_3 <- 
  annual_load_factor %>%
  ggplot()+
  geom_boxplot(aes(y = reorder(primaryspaceusage,desc(primaryspaceusage)), 
               x = annual_load_factor*100,
               fill = as.factor(year)))+
  #geom_label(aes(y = reorder(primaryspaceusage,perc_50), 
  #               x = perc_50,
  #               label = round(perc_50), group = as.factor(year)),
  #           position = position_dodge(0.9),
  #           hjust = 1.5)+
  theme_ai() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))+
  labs(x = "Annual load factor in percent (%)",
       y = "Primary use type",
       fill = "Year")+
  scale_x_continuous(breaks = seq(0,100,5))

ggsave(es_plt_3, filename = "annual load factor in percent by primary use type.png",
       height = 5.5, 
       width = 15)

##############
# building_df
##############

#############################################
# Total building count by primary space usage
#############################################

total_count_primary_space_usage <-
  building_df %>%
  group_by(primaryspaceusage) %>%
  tally()


es_plt_4 <- 
  total_count_primary_space_usage  %>%
  ggplot()+
  geom_col(aes(y = reorder(primaryspaceusage,desc(primaryspaceusage)), 
               x = n ),fill='#A4A4A4', colour="black")+
  geom_text(aes(y = reorder(primaryspaceusage,desc(primaryspaceusage)), 
               x = n,label = n ), hjust = -0.1, size = 6.5) +
  #geom_label(aes(y = reorder(primaryspaceusage,perc_50), 
  #               x = perc_50,
  #               label = round(perc_50), group = as.factor(year)),
  #           position = position_dodge(0.9),
  #           hjust = 1.5)+
  theme_ai() +
  labs(x = "Total building count",
       y = "Primary use type"
  )+
  scale_x_continuous(breaks = seq(0,400,25))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "black"))

ggsave(es_plt_4, filename = "total building count by primary use type.png",
       height = 5.5, 
       width = 15)



#############################################
# building size as boxplots!
#############################################

es_plt_4_2 <- 
  ggplot( building_df, aes(y = reorder(primaryspaceusage,desc(primaryspaceusage)), 
                           x = sqft/1000))+
  geom_boxplot(fill='#A4A4A4')+
  theme_ai() +
  labs(x = "Square feet in thousands",
       y = "Primary use type")+
  scale_x_continuous(breaks = seq(0,1000,100))+
  theme(
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"))

es_plt_4_2_2 <- 
  ggplot( building_df, aes( y = ..count../sum(..count..)*100,
                            x = sqft/1000))+
  geom_histogram(fill='#A4A4A4', colour = "black", binwidth = 25)+
  theme_ai() +
  labs(y = "Fraction of buildings",
       x = "Square feet in thousands")+
  theme(
   # panel.grid.major.y = element_blank(),
    axis.text.y = element_text(colour = "black"),
    axis.text.x = element_text(colour = "black"))+
  scale_x_continuous(breaks = seq(0,1000,50))+
  scale_y_continuous(breaks = seq(0,25,2))

es_plt_4_22 <- 
  building_df %>%
  filter(!is.na(sqft)) %>%
  group_by(primaryspaceusage) %>%
  tally() %>%
  ggplot()+
  geom_col(aes(y = reorder(primaryspaceusage,desc(primaryspaceusage)), 
               x = n ),fill='#A4A4A4', colour="black")+
  #geom_label(aes(y = reorder(primaryspaceusage,perc_50), 
  #               x = perc_50,
  #               label = round(perc_50), group = as.factor(year)),
  #           position = position_dodge(0.9),
  #           hjust = 1.5)+
  geom_text(aes(y = reorder(primaryspaceusage,desc(primaryspaceusage)), 
                x = n, label = n ), size = 6, hjust = -0.1)+
  theme_ai() +
  labs(x = "Total building count",
       y = "Primary use type"
  )+
  scale_x_continuous(breaks = seq(0,400,25), limits = c(0,400))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.ticks.x = element_blank())

ggsave(es_plt_4_2, filename = "building size boxplot by primary use type.png",
       height = 5.5, 
       width = 15)

ggsave(es_plt_4_2_2, filename = "building size histogram by primary use type.png",
       height = 5.5, 
       width = 15)

ggsave(es_plt_4_22, filename = "building size count by primary use type.png",
       height = 5.5, 
       width = 15)


#############################################
# Distribution of building age as boxplots!
#############################################

es_plt_4_3 <- 
  ggplot( building_df, aes(y = reorder(primaryspaceusage,desc(primaryspaceusage)), 
                           x = yearbuilt))+
  geom_boxplot(fill='#A4A4A4')+
  theme_ai() +
  labs(x = "Year",
       y = "Primary use type")+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(colour = "black"),
    axis.text.x = element_text(colour = "black"))+
  scale_x_continuous(breaks = seq(1900,2016,10))

es_plt_4_3_3 <- 
  ggplot( building_df, aes( y = ..count../sum(..count..)*100,
                           x = yearbuilt))+
  geom_histogram(fill='#A4A4A4', colour = "black", binwidth = 10)+
  theme_ai() +
  labs(y = "Fraction of buildings",
       x = "Year")+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(colour = "black"),
    axis.text.x = element_text(colour = "black"))+
  scale_x_continuous(breaks = seq(1900,2016,10))+
  scale_y_continuous(breaks = seq(0,15,2))

es_plt_4_4 <- 
  building_df %>%
  filter(!is.na(yearbuilt)) %>%
  group_by(primaryspaceusage) %>%
  tally() %>%
  ggplot()+
  geom_col(aes(y = reorder(primaryspaceusage,desc(primaryspaceusage)), 
               x = n ),fill='#A4A4A4', colour="black")+
  #geom_label(aes(y = reorder(primaryspaceusage,perc_50), 
  #               x = perc_50,
  #               label = round(perc_50), group = as.factor(year)),
  #           position = position_dodge(0.9),
  #           hjust = 1.5)+
  geom_text(aes(y = reorder(primaryspaceusage,desc(primaryspaceusage)), 
                 x = n, label = n ), size = 6, hjust = -0.1)+
  theme_ai() +
  labs(x = "Total building count",
       y = "Primary use type"
  )+
  scale_x_continuous(breaks = seq(0,400,25), limits = c(0,235))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.ticks.x = element_blank())

ggsave(es_plt_4_3_3, filename = "building age histogram.png",
       height = 5.5, 
       width = 15)

ggsave(es_plt_4_3, filename = "building age boxplot by primary use type.png",
       height = 5.5, 
       width = 15)

ggsave(es_plt_4_4, filename = "building age count by primary use type.png",
       height = 5.5, 
       width = 15)

#############################################
# building locations
#############################################

total_count_primary_space_usage <-
  building_df %>%
  group_by(primaryspaceusage) %>%
  tally()

register_google(key = "AIzaSyC7LxDSt8IQxwG0_wk-UkC9qU1qz_aqoec")

es_plt_5 <-
  get_map(location = "America"
          , zoom = 4) %>% 
  ggmap() +
  geom_point(data = building_df, aes(x = lng, y = lat), colour = "red", size = 3)+
  theme_void()

es_plt_6 <-
  get_map(location = 'Europe', zoom = 4) %>% 
  ggmap() +
  geom_point(data = building_df, aes(x = lng, y = lat), colour = "red", size = 3) + 
  theme_void()

ggsave(es_plt_5, filename = "building locations_1.png",
       height = 5.5, 
       width = 10)

ggsave(es_plt_6, filename = "building locations_2.png",
       height = 5.5, 
       width = 10)

# identifying the cities a building resides within 

lat_long_df <- unique(building_df[,c("site_id_kaggle","lat","lng")])
lat_long_df$city <- 0

for(i in seq(1,dim(lat_long_df)[1])){
  lat_long_df$city[i] <- 
    try(revgeocode(location = c(lat_long_df$lng[i],lat_long_df$lat[i])))
}

lat_long_df$city[1] <- "Fitzrovia-London-UK"
lat_long_df$city[2] <- "Tempe-Arizona-USA"
lat_long_df$city[3] <- "Washington-DC-USA"
lat_long_df$city[4] <- "Berkeley-CA-USA"
lat_long_df$city[5] <- "Cardiff-Wales-UK"
lat_long_df$city[6] <- "Princeton-NJ-USA"
lat_long_df$city[7] <- "Ottawa-Ontario-Canada"
lat_long_df$city[9] <- "Austin-TX-USA"
lat_long_df$city[10] <- "Lauwersoog-Netherlands"
lat_long_df$city[11] <- "Minneapolis-MN-USA"

lat_long_df <- lat_long_df %>%
  left_join(unique(building_df[,c("site_id","site_id_kaggle")]))


##################
# weather_df
##################
weather_df <- 
  weather_df %>%
  left_join(lat_long_df) %>%
  mutate(year = year(timestamp),
         month = month(timestamp),
         dayofmonth = day(timestamp))


es_plt_7 <- 
 weather_df %>%
  filter(!is.na(site_id_kaggle)) %>%
  ggplot()+
  geom_boxplot(aes(x = air_temperature, 
                   y = reorder(as.factor(site_id_kaggle),
                               desc(site_id_kaggle)),fill=as.factor(year)))+
  theme_ai() +
  labs(x = "Air temperature in degrees Celsius",
       y = "Site number",
       fill = "Year")+
  #geom_label(aes(y = reorder(primaryspaceusage,perc_50), 
  #               x = perc_50,
  #               label = round(perc_50), group = as.factor(year)),
  #           position = position_dodge(0.9),
  #           hjust = 1.5)+
  theme_ai() +
  theme(axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        legend.text = element_text(colour = "black"),
        panel.grid.major.y = element_blank())+
  scale_x_continuous(breaks = seq(-25,100,5))

es_plt_7_1 <- 
  ggplot()+
  geom_density(data =  weather_df %>%
                   filter(!is.na(site_id_kaggle), year == 2016),
                 aes(x = air_temperature, 
                   y = ..count../sum(..count..) * 100,
                        colour = as.factor(year)),
               size = 2.5
                 )+
  geom_density(data =  weather_df %>%
                   filter(!is.na(site_id_kaggle), year == 2017),
                   aes(x = air_temperature, 
                       y = ..count../sum(..count..) * 100,
                       colour = as.factor(year)),
               size = 2.5
                )+
  theme_ai() +
  labs(x = "Air temperature in degrees Celsius",
       y = "Fraction of data",
       fill = "Year")+
  #geom_label(aes(y = reorder(primaryspaceusage,perc_50), 
  #               x = perc_50,
  #               label = round(perc_50), group = as.factor(year)),
  #           position = position_dodge(0.9),
  #           hjust = 1.5)+
  theme_ai() +
  theme(
    strip.text.x = element_text(size=16),
    strip.background = element_rect(colour="black", fill="gray75"),
    axis.text.y = element_text(colour = "black"),
    axis.text.x = element_text(colour = "black"),
    legend.text = element_text(colour = "black"),
    panel.grid.major.y = element_blank())+
  scale_x_continuous(breaks = seq(-25,100,10))+ 
  scale_y_continuous(breaks = seq(0,1,.05),
                     )+
  facet_wrap(~site_id_kaggle)



es_plt_7_2 <- 
  weather_df %>%
  filter(!is.na(site_id_kaggle)) %>%
  ggplot()+
  geom_boxplot(aes(x = wind_speed, 
                   y = reorder(as.factor(site_id_kaggle),
                               desc(site_id_kaggle)),fill=as.factor(year)))+
  theme_ai() +
  labs(x = "Wind speed in metres per second",
       y = "Site number",
       fill = "Year")+
  #geom_label(aes(y = reorder(primaryspaceusage,perc_50), 
  #               x = perc_50,
  #               label = round(perc_50), group = as.factor(year)),
  #           position = position_dodge(0.9),
  #           hjust = 1.5)+
  theme_ai() +
  theme(axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        legend.text = element_text(colour = "black"),
        panel.grid.major.y = element_blank()
        )+
  scale_x_continuous(breaks = seq(-25,100,2.5))

ggsave(es_plt_7, filename = "site id median air temperature by year.png",
       height = 5.5, 
       width = 15)

ggsave(es_plt_7_1, filename = "site id air temperature histogram by year.png",
       height = 5.5, 
       width = 15)

ggsave(es_plt_7_2, filename = "site id wind speed by year.png",
       height = 5.5, 
       width = 15)

########################################
# cluster analysis for electricity data
########################################

# The purpose of this analysis is to better understand how each 
# building in the dataset consumes at a daily-level. Are there 
# generalisable patterns that emerge within buildings? Do buildings
# of the same primary use type share similar daily load consumption 
# patterns? What fraction of patterns do they share?

# writing a function to calculate a variety of cluster metrics
# for each building in the dataset.

set.seed(1234) # setting random seed

building_id_list <- unique(electricity_df$building_id_kaggle)

hours<- seq(0,23)

cluster_results <- list()

count <- 1

for(i in building_id_list){
  
  message(count," of ", length(building_id_list))
  
  temp <- filter(electricity_df, building_id_kaggle == i) %>%
    select(timestamp,hour, meter_reading) %>%
    mutate(timestamp = as.Date(timestamp)) %>%
    spread(key = hour, value = meter_reading) %>%
    select(-timestamp)
  
  temp <- temp/rowSums(temp,na.rm = TRUE)
  temp <- temp[complete.cases(temp),]
  
  cluster_results[[count]] <- try(NbClust(temp ,method = "kmeans"))
  
  count <- count + 1
}

#save.image("exploratory analysis_2020-06-09")

# pulling together cluster results for each builing id

optimal_cluster_center_values <- list()
for(i in 1:length(building_id_list)){
  print(i)
  optimal_cluster_center_values[[i]] <- 
    ifelse(try(max(cluster_results[[i]]$Best.partition)),max(cluster_results[[i]]$Best.partition),0)
  
}


building_cluster_values <- 
  data.frame(building_id = building_id_list, 
             cluster_values = unlist(optimal_cluster_center_values))

#save.image("exploratory analysis_2020-06-09")

# several buildings were unable to produce cluster results using nbclust for one reason
# or another so we are using the optimal cluster results package in clusterR.

#load("exploratory analysis_2020-06-09")

na_building_list <- filter(building_cluster_values, is.na(cluster_values)) %>%
  select(building_id)

##########################################################################
# ran into an issue with building id 740 it has repeating values
# across the entire time series so it will be removed from the analysis. 
##########################################################################

electricity_df <- filter(electricity_df, !(building_id_kaggle %in% 740))

na_building_list <- filter(na_building_list, !(building_id %in% 740))

# this building just won't produce a valid cluster result so I am removing
# it from the cluster analysis but keeping it in the overall project

na_building_list <- filter(na_building_list, !(building_id %in% 854))


# creating list to hold silhouette values and a counter
silhouette_value <- list()
count <- 1

for(i in na_building_list$building_id){
  
  message(count," of ", length(na_building_list$building_id))
  
  temp <- filter(electricity_df, building_id_kaggle == i) %>%
    select(timestamp,hour, meter_reading) %>%
    mutate(timestamp = as.Date(timestamp)) %>%
    spread(key = hour, value = meter_reading) %>%
    select(-timestamp)
  
  temp <- temp/rowSums(temp,na.rm = TRUE)
  temp <- temp[complete.cases(temp),]
  
  silhouette_value[[count]] <- 
    try(which.max(Optimal_Clusters_KMeans(temp, max_clusters = 15, 
                                          plot_clusters = F,
                                          criterion = 'silhouette',
                                          num_init = 25,
                                          initializer = 'kmeans++')))
  count <- count + 1
}


# combining the cluster value results from the buildings in the
# na_building_list back with the building_cluster_values data-
# frame. 

na_building_cluster_values <-
  data.frame(building_id = na_building_list$building_id,
             cluster_values = unlist(silhouette_value))

building_cluster_values <- 
  rbind(filter(building_cluster_values,!is.na(cluster_values)),
        na_building_cluster_values)

# running kmeans++ algo on each building 
k_mean_cluster_results <- list()
count <- 1

for(i in 1:length(building_cluster_values$building_id)){
  message(count," of ", length(building_cluster_values$building_id))
  
  temp <- filter(electricity_df, 
                 building_id_kaggle == building_cluster_values$building_id[i]) %>%
    select(timestamp,hour, meter_reading) %>%
    mutate(timestamp = as.Date(timestamp)) %>%
    spread(key = hour, value = meter_reading) %>%
    select(-timestamp)
  
  temp <- temp/rowSums(temp,na.rm = TRUE)
  temp <- temp[complete.cases(temp),]
  
  k_mean_cluster_results[[i]] <- 
    KMeans_rcpp(data = temp,
                clusters = building_cluster_values$cluster_values[i],
                num_init = 25,
                initializer = 'kmeans++',
                seed = 1234)
  count <- count + 1
}

#save.image("exploratory analysis_2020-06-13")
#load("exploratory analysis_2020-06-13")

# plot each set of cluster results for each building id to 
# better understand the results look for each, need to think 
# of a way to compare results across buildings labelled with 
# a similar primary use type. 

# going to create a new df just to store cluster results 

hours <- as.character(hours)

cluster_df <- 
  electricity_df %>%
  select(-timestamp) %>%
  spread(key = hour,value = meter_reading)
  
cluster_df$daily_kwh <- 
  rowSums(cluster_df[,hours],na.rm = T)

cluster_df <- 
  cluster_df %>%
  mutate_at(hours, funs(./daily_kwh))

cluster_df <- cluster_df[complete.cases(cluster_df),]

cluster_building_id_date_final_df <- tibble()

for (i in 1:length(building_cluster_values$building_id)){
  
  temp_building_id <- building_cluster_values$building_id[i]
  
  temp_df <- 
    tibble(building_id = temp_building_id,
           date = unique(filter(cluster_df,building_id_kaggle == temp_building_id)$date),
           cluster_labels = k_mean_cluster_results[[i]]$clusters)
  
  cluster_building_id_date_final_df<-
    rbind(cluster_building_id_date_final_df,
          temp_df)
}

#save.image("exploratory analysis_2020-06-15")

load("exploratory analysis_2020-06-15")

names(cluster_building_id_date_final_df)[1] <- "building_id_kaggle"

cluster_df <- left_join(cluster_df, cluster_building_id_date_final_df)

cluster_df <- cluster_df %>%
  gather(-building_id,-building_id_kaggle,-site_id_kaggle,-year,
         -month,-dayofmonth,-dayofweek,-date,-daily_kwh,-cluster_labels,
         key = hour, value = meter_reading)

cluster_df$hour <- as.integer(cluster_df$hour)

cluster_df$cluster_labels <- as.factor(cluster_df$cluster_labels)

# now lets filter through each building and create a plot that lightly
# colours the lines behind the cluster load profiles. 

for(i in 1:length(building_cluster_values$building_id)){
  message(i)
  temp_building_id <- 
    building_cluster_values$building_id[i]
  
  cluster_average <- 
    cluster_df %>%
    filter(building_id_kaggle == temp_building_id) %>%
    group_by(cluster_labels, hour) %>%
    summarise(avg_daily_meter_reading = mean(meter_reading))
  
  primary_use <- 
    str_replace(filter(building_df, building_id_kaggle == temp_building_id)$primaryspaceusage,
                "/","_")
  
  temp_plt <-
    ggplot()+
    geom_line(data = cluster_df %>%
                filter(building_id_kaggle == temp_building_id),
              aes(x = hour, y = meter_reading,
                  group = interaction(year,month,dayofmonth),
                  color = cluster_labels), alpha = 0.2) +
    geom_line(data = cluster_average, 
              aes(x = hour, y = avg_daily_meter_reading, group = cluster_labels), size = 2) + 
    labs(x = 'hour',
         y = 'fraction of daily electricty consumption (%)',
         color = 'cluster label')+ 
    theme_ai()
  
  temp_plt
  
  try(temp_plt %>%
  ggsave(filename = paste("cluster results for building id_",temp_building_id,"_",primary_use,".png", sep =""),
         height = 5, width = 10))
  
}              

# producing a boxplot of cluster group size by primary use type

cluster_value_boxplot <- 
  left_join(select(building_df, building_id_kaggle,primaryspaceusage),
          building_cluster_values, by = c("building_id_kaggle" = "building_id"))

es_plt_8 <- 
  cluster_value_boxplot %>%
  ggplot()+
  geom_boxplot(aes(x = cluster_values, 
                   y = reorder(as.factor(primaryspaceusage),
                               desc(primaryspaceusage))),fill='#A4A4A4')+
  theme_ai() +
  labs(x = "Cluster group size",
       y = "Primary use type")+
  #geom_label(aes(y = reorder(primaryspaceusage,perc_50), 
  #               x = perc_50,
  #               label = round(perc_50), group = as.factor(year)),
  #           position = position_dodge(0.9),
  #           hjust = 1.5)+
  theme_ai() +
  theme(axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        legend.text = element_text(colour = "black"),
        panel.grid.major.y = element_blank())+
  scale_x_continuous(breaks = seq(0,10,1), limits = c(0,10))

es_plt_8_2 <- 
  cluster_value_boxplot %>%
  ggplot()+
  geom_histogram(aes(x = cluster_values, 
                   y = ..count../sum(..count..)*100),fill='#A4A4A4', color = 'black')+
  theme_ai() +
  labs(x = "Cluster group size",
       y = "Fraction of Dataset (%)")+
  #geom_label(aes(y = reorder(primaryspaceusage,perc_50), 
  #               x = perc_50,
  #               label = round(perc_50), group = as.factor(year)),
  #           position = position_dodge(0.9),
  #           hjust = 1.5)+
  theme_ai() +
  theme(axis.text.y = element_text(colour = "black"),
       axis.text.x = element_text(colour = "black"),
       legend.text = element_text(colour = "black"),
       panel.grid.major.x = element_blank())+
  scale_x_continuous(breaks = seq(0,10,1), limits = c(0,10))+
  scale_y_continuous(breaks = seq(0,100,5))

ggsave(wrap_plots(es_plt_8,es_plt_8_2), filename = "cluster group size boxplot by primary use type.png",
       height = 5.5, 
       width = 15)

# PRODUCING A MUCH CLEANER CLUSTER GROUP PLOT AND 
# CALENDAR PLOT FOR A SINGLE BUILDING FROM THE 
# DATASET.


temp_building_id <- 
  building_cluster_values$building_id[3]

cluster_average <- 
  cluster_df %>%
  filter(building_id_kaggle == temp_building_id) %>%
  group_by(cluster_labels, hour) %>%
  summarise(avg_daily_meter_reading = mean(meter_reading))

primary_use <- 
  str_replace(filter(building_df, building_id_kaggle == temp_building_id)$primaryspaceusage,
              "/","_")

es_plt_9 <-
  ggplot()+
  geom_line(data = cluster_df %>%
              filter(building_id_kaggle == temp_building_id),
            aes(x = hour, y = meter_reading*100,
                group = interaction(year,month,dayofmonth),
                color = cluster_labels), alpha = 0.2) +
  geom_line(data = cluster_average, 
            aes(x = hour, y = avg_daily_meter_reading*100, group = cluster_labels), size = 2) + 
  labs(x = 'Hour',
       y = 'Fraction of daily electricty consumption (%)',
       color = 'Cluster group')+ 
  theme_ai()+
  scale_x_continuous(breaks = seq(0,23,1))+
  scale_color_manual(values = c("seagreen3","royalblue3"))

temp_plt 

temp_df <- cluster_df %>%
  filter(building_id_kaggle == temp_building_id) %>%
  select(date,cluster_labels)
  
es_plt_9_2 <- 
  calendarHeatmap(dates = temp_df$date,
                  values = as.factor(temp_df$cluster_labels))

es_plt_9 %>%
  ggsave(filename = "cluster group plot example for a single building_2020-06-19.png",
         height = 5,
         width = 15)

es_plt_9_2 %>%
  ggsave(filename = "calendar map example for a single building_2020-06-19.png",
         height = 5,
         width = 15)

####################################################################
# Clustering cluster load profiles within a primary use type
####################################################################

# For this analysis I looked at each primary use type and identified 
# the total number of cluster groupings that would achieve a WCSS/TSS
# of approximately 80%. This means that 80% of the variation in the data
# is explained by our cluster groupings.

primary_use_type <- 
  sort(primary_use_type)

within_primary_type_cluster_results <- list()

for(i in 1:length(primary_use_type)){

temp_df <- left_join(cluster_df,building_df[,c('building_id_kaggle','primaryspaceusage')]) %>%
  filter(primaryspaceusage == primary_use_type[i]) %>%
  spread(key = hour, value = meter_reading)

cluster_values <- 
  c(30, 35, 25, 20, 40, 25, 25, 45, 30, 55, 25, 15, 15, 15, 5, 5)

temp <- KMeans_rcpp(data =round(temp_df[,hours],4),
            clusters = cluster_values[i],
            num_init = 5,
            initializer = 'kmeans++',
            seed = 1234) 

within_primary_type_cluster_results[[i]] <- temp
}

save.image("exploratory analysis_2020-06-22")


for(i in 1:length(primary_use_type)){

temp_df <- left_join(cluster_df,building_df[,c('building_id_kaggle','primaryspaceusage')]) %>%
  filter(primaryspaceusage == primary_use_type[i])

total_buildings <- 
  length(unique(temp_df$building_id_kaggle))

temp_df <- 
  temp_df %>%
  spread(key = hour, value = meter_reading)


temp_df<-
  cbind(temp_df,
        primary_type_cluster_values = 
          within_primary_type_cluster_results[[i]]$clusters)

unique_load_profiles <- 
  unique(temp_df[,c("building_id_kaggle","primary_type_cluster_values")]) %>% 
  group_by(building_id_kaggle) %>%
  tally()

top_10_cluster_values <-
temp_df %>%
  group_by(primary_type_cluster_values) %>%
  tally() %>%
  arrange(desc(n)) %>%
  mutate(perc_n = n/sum(n)) %>%
  top_n(wt = n, 10)

top_10_cluster_values <- 
  top_10_cluster_values[1:10,]

temp_df_names <- 
  names(temp_df)[!names(temp_df) %in% hours]

temp_df <- 
  temp_df %>%
  filter(primary_type_cluster_values %in% 
           top_10_cluster_values$primary_type_cluster_values) %>%
  gather(-c(temp_df_names),key = hour,value = meter_reading)

temp_df$hour <- as.integer(temp_df$hour)

# what are the number of builings that fall into these top 10 load
# profiles?

num_buildings <- 
  length(unique(temp_df$building_id_kaggle))

avg_temp_df <- temp_df %>%
  group_by(primary_type_cluster_values,hour) %>%
  summarise(avg_meter_reading = mean(meter_reading))


avg_temp_df$hour <- as.integer(avg_temp_df$hour)
avg_temp_df$primary_type_cluster_values <- as.factor(avg_temp_df$primary_type_cluster_values)

top_10_cluster_values <- 
  top_10_cluster_values %>%
  arrange(primary_type_cluster_values)

cluster_labels <- str_c ("Cluster " , seq(1,length(top_10_cluster_values$perc_n)),": ", round(top_10_cluster_values$perc_n,2)*100,"% of load shapes")

levels(avg_temp_df$primary_type_cluster_values) <- cluster_labels 

es_plt_9 <- 
  ggplot()+
  # geom_line(data = temp_df,
  #           aes(x = hour , 
  #               y = meter_reading, 
  #               group = interaction(year,month,dayofmonth,building_id_kaggle),
  #               color = as.factor(primary_type_cluster_values)), alpha = 0.2)+
  geom_line(data = avg_temp_df,
            aes(x = hour, y = avg_meter_reading * 100, 
                group = primary_type_cluster_values,
                color = primary_type_cluster_values), size =2
            )+
  theme_ai()+
  facet_wrap(~as.factor(primary_type_cluster_values),nrow = 2,ncol = 5)+
  labs(x = 'Hour',
       y = 'Fraction of daily electricity consumption (%)')+
       # subtitle = paste(num_buildings,"of", 
       #                  total_buildings,primary_use_type[i],
       #                  " buildings captured in the 10 cluster load profiles below"))+
  scale_x_continuous(breaks = seq(0,23,4)) + 
  theme(legend.position = 'none',
        strip.text.x = element_text(size = 14, colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))

ggsave(es_plt_9, 
       filename = paste(str_replace(primary_use_type[i],pattern = "/","_"),"_cluster load profiles.png", sep = ""),
       height = 7.5,
       width = 15)  

#########################################################
#' 2020-08-19 I also want to produce another figure
#' that looks at how many distinct clusters each building
#' has within a primary use type.
#########################################################


es_plt_9_2 <- 
  unique_load_profiles %>%
  ggplot()+
  geom_histogram(aes(x = n, y = cumsum(..count..)/sum(..count..) * 100),
                 binwidth = 1, fill='#A4A4A4', colour = "black")+
  theme_ai()+
  theme(axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"))+
  labs(x = "Cluster group size",
       y = "Fraction of customers")+
  scale_x_continuous(breaks = seq(0,max(unique_load_profiles$n,1)))+
  scale_y_continuous(breaks = seq(0,100,5))

unique_top10_load_profiles <-
  filter(temp_df,primary_type_cluster_values %in% 
         top_10_cluster_values$primary_type_cluster_values)
 
unique_top10_load_profiles <-
  unique(unique_top10_load_profiles[,c("building_id_kaggle","primary_type_cluster_values")]) %>%
  group_by(building_id_kaggle) %>%
  tally()

es_plt_9_3 <- 
  unique_top10_load_profiles %>%
  ggplot()+
  geom_histogram(aes(x = n, y = cumsum(..count..)/sum(..count..) * 100),
                 binwidth = 1, fill='#A4A4A4', colour = "black")+
  theme_ai()+
  theme(axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"))+
  labs(x = "Cluster group size",
       y = "Fraction of customers")+
  scale_x_continuous(breaks = seq(0,max(unique_top10_load_profiles$n,1)))+
  scale_y_continuous(breaks = seq(0,100,5))

ggsave(es_plt_9_2, 
       filename = paste(str_replace(primary_use_type[i],pattern = "/","_"),
                        "_ cumulative distribution for cluster load sizes.png"),
       height = 7.5, 
       width = 15)

ggsave(es_plt_9_3, 
       filename = paste(str_replace(primary_use_type[i],pattern = "/","_"),
                        "_ cumulative distribution for top 10 cluster load sizes.png"),
       height = 7.5, 
       width = 15)

ggsave(wrap_plots(es_plt_9_2,es_plt_9_3), 
       filename = paste(str_replace(primary_use_type[i],pattern = "/","_"),
                        "_ cumulative distribution for cluster load sizes combined.png"),
       height = 7.5, 
       width = 20)


}


save.image("exploratory analysis_2020-08-22_final")
