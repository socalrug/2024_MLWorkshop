movie.data<- read.csv(file = here::here("DATA_SETS/movie_data.csv"), 
                      header=TRUE, sep=",")

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(566222)
sample <- sample(c(TRUE, FALSE), nrow(movie.data),replace=TRUE, prob=c(0.8,0.2))
train<- movie.data[sample,]
test<- movie.data[!sample,]

#BUILDING RANDOM FOREST MULTINOMIAL CLASSIFIER
library(randomForest)
rf.class<- randomForest(as.factor(rating) ~ age + gender + member + nmovies, data=train)

#DISPLAYING FEATURE IMPORTANCE
print(importance(rf.class,type=2)) 

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
predclass<- predict(rf.class, newdata=test)
test<- cbind(test,predclass)

accuracy<- c()
for (i in 1:nrow(test))
  accuracy[i]<- ifelse(test$rating[i]==test$predclass[i],1,0)

print(accuracy<- mean(accuracy))

