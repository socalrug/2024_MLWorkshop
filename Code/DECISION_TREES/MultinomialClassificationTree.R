movie.data<- read.csv(file = here::here("DATA_SETS/movie_data.csv"), 
                                        header=TRUE, sep=",")

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(566222)
sample <- sample(c(TRUE, FALSE), nrow(movie.data),replace=TRUE, prob=c(0.8,0.2))
train<- movie.data[sample,]
test<- movie.data[!sample,]

#FITTING PRUNED MULTINOMIAL CLASSIFICATION TREE 
library(rpart)
tree.gini<- rpart(rating ~ age + gender + member + nmovies, 
data=train, method="class", parms=list(split="Gini"), maxdepth=4)

#PLOTTING FITTED TREE
library(rpart.plot)
rpart.plot(tree.gini, type=3)

#COMPUTING PREDICTED VALUES FOR TESTING DATA
pred.prob<- predict(tree.gini, test)

#DETERMINING PREDICTED CLASSES
test<- cbind(test, pred.prob)
test$maxprob<- pmax(test$'very bad',test$'bad',test$'okay',
test$'good',test$'very good')

test$predclass<- ifelse(test$maxprob==test$'very bad', 'very bad', 
ifelse(test$maxprob==test$'bad','bad',
ifelse(test$maxprob==test$'okay','okay',
ifelse(test$maxprob==test$'good','good','very good'))))

match<- c()
for (i in 1:nrow(test))
  match[i]<- ifelse(test$rating[i]==test$predclass[i],1,0)

print(accuracy<- mean(match))
