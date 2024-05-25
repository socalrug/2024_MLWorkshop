tsla.data <- read.csv(file = here::here("DATA_SETS/TSLAStockPrices.csv"), 
                     header=TRUE, sep=",")

tsla.data$Date<- as.Date(tsla.data$Date, format="%Y-%m-%d")
tsla.data<- tsla.data[which(tsla.data$Date> as.Date('2020-01-10')),1:2]

#install.packages("tibbletime")
library(tibbletime) #creates indices for date in time series data
tsla.data_tbl <- as_tbl_time(tsla.data, Date)
#install.packages("anomalize")
library(anomalize) 
library(tidyverse)
tsla.data_tbl %>% time_decompose(Price, method="stl") %>% 
anomalize(remainder, method="iqr") %>% time_recompose() %>% 
plot_anomalies(time_recomposed=TRUE, color_no="navy", 
color_yes="red",fill_ribbon="gray", size_circles=4) + 
labs(title="Anomalies in Daily Closing Prices of Tesla Stock", subtitle="1/2/2020-5/9/2024") 

