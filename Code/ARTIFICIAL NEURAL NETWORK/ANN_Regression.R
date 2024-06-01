housing.data<- read.csv(file = here::here("DATA_SETS/housing_data.csv"),
                        header=TRUE, sep=",")

housing.data$ocean_proximity<- ifelse(housing.data$ocean_proximity=='<1H OCEAN',
1, ifelse(housing.data$ocean_proximity=='INLAND',2, 
ifelse(housing.data$ocean_proximity=='NEAR BAY',3,4)))

#SCALING VARIABLES TO FALL IN [0,1]
library(dplyr)

scale01 <- function(x){
  (x-min(x))/(max(x)-min(x))
}

housing.data.re<- housing.data %>% mutate_all(scale01)

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(346634)
sample <- sample(c(TRUE, FALSE), nrow(housing.data.re), replace=TRUE, 
prob=c(0.8,0.2))
train<- housing.data.re[sample,]
test<-  housing.data.re[!sample,]
test.x<- data.matrix(test[-8])
test.y<- data.matrix(test[8])

#################################################################
#FITTING ANN WITH LOGISTIC ACTIVATION FUNCTION
library(neuralnet)
ann.reg<- neuralnet(median_house_value ~ housing_median_age+total_rooms
+total_bedrooms+population+households+median_income+ocean_proximity,  
data=train, hidden=2, act.fct="logistic") 

#PLOTTING THE DIAGRAM
plot(ann.reg)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA
pred.y.re<- predict(ann.reg, test.x)

#switching back to the original units
y<- housing.data[,8]
pred.y<- pred.y.re*(max(y)-min(y))+min(y)
true.y<- test.y*(range(max(y)-min(y))+min(y))


#computing accuracy within 10%
accuracy10<- ifelse(abs(true.y-pred.y)<0.10*true.y,1,0) 

#computing accuracy within 15%
accuracy15<- ifelse(abs(true.y-pred.y)<0.15*true.y,1,0)

#computing accuracy within 20%
accuracy20<- ifelse(abs(true.y-pred.y)<0.20*true.y,1,0)

print('Prediction Accuracy')
print(paste('within 10%:', round(mean(accuracy10),4)))
print(paste('within 15%:', round(mean(accuracy15),4)))
print(paste('within 20%:', round(mean(accuracy20),4)))

#PLOTTING ACTUAL AND PREDICTED VALUES FOR TESTING DATA
x<- 1:length(test.y)
plot(x, true.y, type="l", lwd=2, col="orange", main="ANN Regression with
Logistic Activation Function", panel.first=grid())
lines(x, pred.y, lwd=2, col="purple")
points(x,true.y, pch=16, col="orange")
points(x, pred.y, pch=16, col="purple")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,col=c("orange","purple"))


#################################################################
#FITTING ANN WITH TANH ACTIVATION FUNCTION

ann.reg<- neuralnet(median_house_value ~ housing_median_age+total_rooms
+total_bedrooms+population+households+median_income+ocean_proximity,  
data=train, hidden=2, act.fct="tanh", stepmax=1e6) 

#PLOTTING THE DIAGRAM
plot(ann.reg)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA
pred.y.re<- predict(ann.reg, test.x)

#switching back to the original units
y<- housing.data[,8]
pred.y<- pred.y.re*(max(y)-min(y))+min(y)
true.y<- test.y*(range(max(y)-min(y))+min(y))


#computing accuracy within 10%
accuracy10<- ifelse(abs(true.y-pred.y)<0.10*true.y,1,0) 

#computing accuracy within 15%
accuracy15<- ifelse(abs(true.y-pred.y)<0.15*true.y,1,0)

#computing accuracy within 20%
accuracy20<- ifelse(abs(true.y-pred.y)<0.20*true.y,1,0)

print('Prediction Accuracy')
print(paste('within 10%:', round(mean(accuracy10),4)))
print(paste('within 15%:', round(mean(accuracy15),4)))
print(paste('within 20%:', round(mean(accuracy20),4)))

#PLOTTING ACTUAL AND PREDICTED VALUES FOR TESTING DATA
x<- 1:length(test.y)
plot(x, true.y, type="l", lwd=2, col="green", main="ANN Regression with
Tanh Activation Function", panel.first=grid())
lines(x, pred.y, lwd=2, col="blue")
points(x,true.y, pch=16, col="green")
points(x, pred.y, pch=16, col="blue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("green","blue"))

