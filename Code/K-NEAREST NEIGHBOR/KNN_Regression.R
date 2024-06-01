housing.data<- read.csv(file = here::here("DATA_SETS/housing_data.csv"),
                        header=TRUE, sep=",")

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(880352)
sample <- sample(c(TRUE, FALSE), nrow(housing.data), replace=TRUE, prob=c(0.8,0.2))
train<- housing.data[sample,]
test<- housing.data[!sample,]

train.x<- data.matrix(train[-8])
train.y<- data.matrix(train[8])
test.x<- data.matrix(test[-8])
test.y<- data.matrix(test[8])

#TRAINING K-NEAREST NEIGHBOR REGRESSION 
library(caret)  #classification and regression training
print(train(median_house_value ~ ., data=train, method="knn"))

#FITTING KNN REGRESSION
knn.reg<- knnreg(train.x, train.y, k=5)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
pred.y<- predict(knn.reg, test.x)

#accuracy within 10%
accuracy10<- ifelse(abs(test.y-pred.y)<0.10*test.y,1,0) 
print(mean(accuracy10))

#accuracy within 15%
accuracy15<- ifelse(abs(test.y-pred.y)<0.15*test.y,1,0) 
print(mean(accuracy15))

#accuracy within 20%
accuracy20<- ifelse(abs(test.y-pred.y)<0.20*test.y,1,0) 
print(mean(accuracy20))

#PLOTTING ACTUAL AND PREDICTED VALUES FOR TESTING DATA
x<- 1:length(test.y)
plot(x, test.y, type="l", lwd=2, col="magenta", main="KNN Regression", 
panel.first=grid())
lines(x, pred.y, lwd=2, col="dodgerblue")
points(x,test.y, pch=16, col="magenta")
points(x, pred.y, pch=16, col="dodgerblue")
legend("topright", c("actual", "predicted"), lty=1, lwd=2,
col=c("magenta","dodgerblue"))






