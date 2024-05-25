housing.data<- read.csv(file = here::here("DATA_SETS/housing_data.csv"),
                                          header=TRUE, sep=",")

housing.data$ocean_proximity<- ifelse(housing.data$ocean_proximity=='<1H OCEAN',
1, ifelse(housing.data$ocean_proximity=='INLAND',2, 
ifelse(housing.data$ocean_proximity=='NEAR BAY',3,4)))

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(234564)
sample <- sample(c(TRUE, FALSE), nrow(housing.data), replace=TRUE, 
prob=c(0.8,0.2))
train<- housing.data[sample,]
test<-  housing.data[!sample,]
test.x<- data.matrix(test[-8])
test.y<- data.matrix(test[8])

#install.packages("e1071")
library(e1071)

######################################################
#FITTING SVR WITH LINEAR KERNEL
######################################################
svm.reg<- svm(median_house_value ~ housing_median_age+total_rooms
+total_bedrooms+population+households+median_income+ocean_proximity, 
data=train, kernel="linear")

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
pred.y<- predict(svm.reg, test.x)

#accuracy within 10%
accuracy10<- ifelse(abs(test.y-pred.y)<0.10*test.y,1,0) 

#accuracy within 15%
accuracy15<- ifelse(abs(test.y-pred.y)<0.15*test.y,1,0) 

#accuracy within 20%
accuracy20<- ifelse(abs(test.y-pred.y)<0.20*test.y,1,0)

print('Linear Kernel')
print(paste('within 10%:', round(mean(accuracy10),4)))
print(paste('within 15%:', round(mean(accuracy15),4)))
print(paste('within 20%:', round(mean(accuracy20),4)))

#PLOTTING ACTUAL AND PREDICTED VALUES FOR TESTING DATA
x<- 1:length(test.y)
plot(x, test.y, type="l", lwd=2, col="magenta", main="SVM Regression with 
Linear Kernel", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
points(x,test.y, pch=16, col="magenta")
points(x, pred.y, pch=16, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("magenta","dodgerblue"))

#######################################################
#FITTING SVR WITH POLYNOMIAL KERNEL
#######################################################
svm.reg<- svm(median_house_value ~ housing_median_age+total_rooms
+total_bedrooms+population+households+median_income+ocean_proximity, 
data=train, kernel="poly")

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
pred.y<- predict(svm.reg, test.x)

#accuracy within 10%
accuracy10<- ifelse(abs(test.y-pred.y)<0.10*test.y,1,0) 

#accuracy within 15%
accuracy15<- ifelse(abs(test.y-pred.y)<0.15*test.y,1,0) 

#accuracy within 20%
accuracy20<- ifelse(abs(test.y-pred.y)<0.20*test.y,1,0)

print('Polynomial Kernel')
print(paste('within 10%:', round(mean(accuracy10),4)))
print(paste('within 15%:', round(mean(accuracy15),4)))
print(paste('within 20%:', round(mean(accuracy20),4)))

#PLOTTING ACTUAL AND PREDICTED VALUES FOR TESTING DATA
x<- 1:length(test.y)
plot(x, test.y, type="l", lwd=2, col="magenta", main="SVM Regression with 
Polynomial Kernel", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
points(x,test.y, pch=16, col="magenta")
points(x, pred.y, pch=16, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("magenta","dodgerblue"))

#######################################################
#FITTING SVR WITH RADIAL KERNEL
#######################################################
svm.reg<- svm(median_house_value ~ housing_median_age+total_rooms
+total_bedrooms+population+households+median_income+ocean_proximity, 
data=train, kernel="radial")

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
pred.y<- predict(svm.reg, test.x)

#accuracy within 10%
accuracy10<- ifelse(abs(test.y-pred.y)<0.10*test.y,1,0) 

#accuracy within 15%
accuracy15<- ifelse(abs(test.y-pred.y)<0.15*test.y,1,0) 

#accuracy within 20%
accuracy20<- ifelse(abs(test.y-pred.y)<0.20*test.y,1,0)

print('Radial Kernel')
print(paste('within 10%:', round(mean(accuracy10),4)))
print(paste('within 15%:', round(mean(accuracy15),4)))
print(paste('within 20%:', round(mean(accuracy20),4)))

#PLOTTING ACTUAL AND PREDICTED VALUES FOR TESTING DATA
x<- 1:length(test.y)
plot(x, test.y, type="l", lwd=2, col="magenta", main="SVM Regression with 
Radial Kernel", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
points(x,test.y, pch=16, col="magenta")
points(x, pred.y, pch=16, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("magenta","dodgerblue"))

#####################################################
#FITTING SVR WITH SIGMOID KERNEL
#####################################################
svm.reg<- svm(median_house_value ~ housing_median_age+total_rooms
+total_bedrooms+population+households+median_income+ocean_proximity, 
data=train, kernel="sigmoid")

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
pred.y<- predict(svm.reg, test.x)

#accuracy within 10%
accuracy10<- ifelse(abs(test.y-pred.y)<0.10*test.y,1,0) 

#accuracy within 15%
accuracy15<- ifelse(abs(test.y-pred.y)<0.15*test.y,1,0) 

#accuracy within 20%
accuracy20<- ifelse(abs(test.y-pred.y)<0.20*test.y,1,0)

print('Sigmoid Kernel')
print(paste('within 10%:', round(mean(accuracy10),4)))
print(paste('within 15%:', round(mean(accuracy15),4)))
print(paste('within 20%:', round(mean(accuracy20),4)))

#PLOTTING ACTUAL AND PREDICTED VALUES FOR TESTING DATA
x<- 1:length(test.y)
plot(x, test.y, type="l", lwd=2, col="magenta", main="SVM Regression 
with Sigmoid Kernel", panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
points(x,test.y, pch=16, col="magenta")
points(x, pred.y, pch=16, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("magenta","dodgerblue"))