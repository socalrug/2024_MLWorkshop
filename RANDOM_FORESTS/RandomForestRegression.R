#install.packages("randomForest")
library(randomForest)

housing.data<- read.csv(file = here::here("DATA_SETS/housing_data.csv"),
                        header=TRUE, sep=",")

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(902881)
sample <- sample(c(TRUE, FALSE), nrow(housing.data), 
replace=TRUE, prob=c(0.8,0.2))
train<- housing.data[sample,]
test<- housing.data[!sample,]

#BUILDING RANDOM FOREST REGRESSION
rf.reg<- randomForest(median_house_value ~ housing_median_age 
+ total_rooms	+ total_bedrooms + population + households	
+ median_income + ocean_proximity, data=train)

#DISPLAYING FEATURE IMPORTANCE
print(importance(rf.reg)) 

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
P_median_house_value<- predict(rf.reg, newdata=test)

#accuracy within 10%
accuracy10<- ifelse(abs(test$median_house_value-P_median_house_value)<0.10*test$median_house_value,1,0) 
print(accuracy10<- mean(accuracy10))

#accuracy within 15%
accuracy15<- ifelse(abs(test$median_house_value-P_median_house_value)<0.15*test$median_house_value,1,0)
print(accuracy15<- mean(accuracy15))

#accuracy within 20%
accuracy20<- ifelse(abs(test$median_house_value-P_median_house_value)<0.20*test$median_house_value,1,0)
print(accuracy20<- mean(accuracy20))

