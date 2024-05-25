#install.packages("xgboost")
library(xgboost)

housing.data<- read.csv(file = here::here("DATA_SETS/housing_data.csv"), 
header=TRUE, sep=",")

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(9000345)
sample <- sample(c(TRUE, FALSE), nrow(housing.data), replace=TRUE, prob=c(0.8,0.2))
train<- housing.data[sample,]
test<- housing.data[!sample,]

train.x<- data.matrix(train[-8])
train.y<- data.matrix(train[8])
test.x<- data.matrix(test[-8])
test.y<- data.matrix(test[8])

#FITTING EXTREME GRADIENT BOOSTED REGRESSION TREE
xgb.reg<- xgboost(data=train.x, label=train.y, 
max.depth=6, eta=0.01, subsample=0.8, colsample_bytree=0.5, 
nrounds=1000, objective="reg:linear")#eta=learning rate
#colsample_bytree defines what percentage of features (columns) 
#will be used for building each tree

#DISPLAYING FEATURE IMPORTANCE
print(xgb.importance(colnames(train.x), model=xgb.reg))

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
pred.y<- predict(xgb.reg, test.x)

#accuracy within 10%
accuracy10<- ifelse(abs(test.y-pred.y)<0.10*test.y,1,0) 
print(mean(accuracy10))

#accuracy within 15%
accuracy15<- ifelse(abs(test.y-pred.y)<0.15*test.y,1,0) 
print(mean(accuracy15))

#accuracy within 20%
accuracy20<- ifelse(abs(test.y-pred.y)<0.20*test.y,1,0) 
print(mean(accuracy20))


