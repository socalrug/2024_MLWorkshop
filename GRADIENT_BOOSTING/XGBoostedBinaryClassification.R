#install.packages("xgboost")
library(xgboost)

pneumonia.data<- read.csv(file = here::here("DATA_SETS/pneumonia_data.csv"),
                          header=TRUE, sep=",")

pneumonia.data$pneumonia<- ifelse(pneumonia.data$pneumonia=="yes",1,0)

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(447558)
sample <- sample(c(TRUE, FALSE), nrow(pneumonia.data), 
replace=TRUE, prob=c(0.8,0.2))
train<- pneumonia.data[sample,]
test<- pneumonia.data[!sample,]

train.x<- data.matrix(train[-5])
train.y<- data.matrix(train[5])
test.x<- data.matrix(test[-5])
test.y<- data.matrix(test[5])

#FITTING GRADIENT BOOSTED BINARY CLASSIFIER
xgb.class<- xgboost(data=train.x, label=train.y, 
max.depth=6, eta=0.1, subsample=0.8, colsample_bytree=0.5, 
nrounds=1000, objective="binary:logistic")

#DISPLAYING FEATURE IMPORTANCE
print(xgb.importance(colnames(train.x), model=xgb.class))

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
pred.prob<- predict(xgb.class, test.x)

len<- length(pred.prob)
pred.pneumonia<- c()
match<- c()
for (i in 1:len){
  pred.pneumonia[i]<- ifelse(pred.prob[i]>=0.5, 1,0)
  match[i]<- ifelse(test.y[i]==pred.pneumonia[i], 1,0)
}
print(prop<- mean(match))
