library(xgboost)

movie.data<- read.csv(file = here::here("DATA_SETS/movie_data.csv"),
                      header=TRUE, sep=",")
            
#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(103321)
sample <- sample(c(TRUE, FALSE), nrow(movie.data), replace=TRUE, prob=c(0.8,0.2))
train<- movie.data[sample,]
test<- movie.data[!sample,]

train.x<- data.matrix(train[-5])
train.y<- data.matrix(train[5])
train.y<- train.y-1 #must range between 0 and 4 for prediction
test.x<- data.matrix(test[-5])
test.y<- data.matrix(test[5])
test.y<- test.y-1

#FITTING GRADIENT BOOSTED MULTINOMIAL CLASSIFIER
xgb.mclass<- xgboost(data=train.x, label=train.y, 
max.depth=6, eta=0.1, subsample=0.8, colsample_bytree=0.5, 
nrounds=1000, num_class=5, objective="multi:softprob")

#DISPLAYING FEATURE IMPORTANCE
print(xgb.importance(colnames(train.x), model=xgb.mclass))

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
pred.prob <- predict(xgb.mclass, test.x, reshape=TRUE)
pred.prob<- as.data.frame(pred.prob)
colnames(pred.prob)<- 0:4

pred.class<- apply(pred.prob, 1, function(x) 
colnames(pred.prob)[which.max(x)])

match<- c()
n<- length(test.y)
for (i in 1:n) {
  match[i]<- ifelse(pred.class[i]==as.character(test.y[i]),1,0)
}

print(accuracy<- mean(match))