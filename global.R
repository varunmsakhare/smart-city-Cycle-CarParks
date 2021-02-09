library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(dplyr)
library(DT)
library(ggplot2)
library(plyr)
library(data.table)
library(lubridate)
library(gganimate)
library(streamgraph)
library(ggExtra)
library(plotly)
library(tidyverse)
library(ggridges)
library(randomForest)
library(Metrics)

################################################## Cycle ##################################################  

OID <- read.csv("Cycle_weather.csv")
OID[,"Date"] <- as.Date(OID$Date,"%Y-%m-%d")
OID$Temperature <- round(OID$Temperature)
OID$Months <- factor(OID$Months, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))


Cycle_Weather <- read.csv("Cycle_Weather.csv")


Cycle_Weather$Year <- as.factor(Cycle_Weather$Year)

#####
first <- OID %>% filter(Year == 2016)
nrow(count(first$Name))
second <- OID %>% filter(Year == 2017)
nrow(count(second$Name))
third <- OID %>% filter(Year == 2018)
nrow(count(third$Name))
fourth <- OID %>% filter(Year == 2019)
nrow(count(fourth$Name))
fifth <- OID %>% filter(Year == 2020)
nrow(count(fifth$Name))

###
OID_Weekday <- OID %>% filter(Week == "Weekday")
OID_Weekend <- OID %>% filter(Week == "Weekend")


######## MODEL ON CYCLE
train_set <- read.csv('bike_train.csv')
test_set <- read.csv('bike_test.csv')

test_set$Temperature <- round(test_set$Temperature)
test_set$Humidity <- round(test_set$Humidity)
test_set$WindSpeed <- round(test_set$WindSpeed)


#Importing model
model_rf <- readRDS(file = './rf.rda')
y_pred = predict(model_rf, newdata = test_set)
mae_rf = mae(test_set$Number, y_pred)
rmse_rf = rmse(test_set$Number, y_pred)


################################################## Parking ##################################################  


Parking <- read.csv("Parking_Weather_Avg.csv")
Parking[,"Date"] <- as.Date(Parking$Date,"%Y-%m-%d")
Parking$Available <- round(Parking$Available)
Parking$Occupied <- round(Parking$Occupied)


######## MODEL ON PARKING
parking_train_set <- read.csv('parking_train.csv')
parking_test_set <- read.csv('parking_test.csv')

#parking_test_set$Temperature <- round(parking_test_set$Temperature)
#parking_test_set$Humidity <- round(parking_test_set$Humidity)
#parking_test_set$WindSpeed <- round(parking_test_set$WindSpeed)


#Importing model
model_parking_rf <- readRDS(file = './parking_rf.rda')
parking_y_pred = predict(model_parking_rf, newdata = parking_test_set)
parking_mae_rf = mae(parking_test_set$Percentage_Occupied[[10]], parking_y_pred)
parking_rmse_rf = rmse(parking_test_set$Percentage_Occupied[[10]], parking_y_pred)

