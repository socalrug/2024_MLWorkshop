movie.data<- read.csv(file = here::here("DATA_SETS/movie_data.csv"), 
                                        header=TRUE, sep=",")
            
movie.data$gender<- ifelse(movie.data$gender=='M',1,0)
movie.data$member<- ifelse(movie.data$member=='yes',1,0)

movie.data$rating<- ifelse(movie.data$rating=='very bad',1,
ifelse(movie.data$rating=='bad',2,ifelse(movie.data$rating=='okay',3,
ifelse(movie.data$rating=='good',4,5))))

#SCALING VARIABLES TO FALL IN [0,1]
library(dplyr)

scale01 <- function(x){
  (x-min(x))/(max(x)-min(x))
}

movie.data<- movie.data %>% mutate_all(scale01)

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(100001)
sample <- sample(c(TRUE, FALSE), nrow(movie.data), replace=TRUE, prob=c(0.8,0.2))
train<- movie.data[sample,]
test<- movie.data[!sample,]

train.x<- data.matrix(train[-5])
train.y<- data.matrix(train[5])
test.x<- data.matrix(test[-5])
test.y<- data.matrix(test[5])

######################################################################

#FITTING ANN WITH LOGISTIC ACTIVATION FUNCTION

library(neuralnet)
ann.mclass<- neuralnet(as.factor(rating) ~ age + gender + member + nmovies,
data=train, hidden=3, act.fct="logistic") 

#PLOTTING THE DIAGRAM
plot(ann.mclass)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA
pred.prob <- predict(ann.mclass, test.x)
pred.prob<- as.data.frame(pred.prob)

colnames(pred.prob)<- c(0, 0.25, 0.5, 0.75, 1)
 
pred.class<- apply(pred.prob, 1, function(x) colnames(pred.prob)[which.max(x)])

match<- c()
for (i in 1:length(test.y)) {
  match[i]<- ifelse(pred.class[i]==as.character(test.y[i]),1,0)
}

print(accuracy<- mean(match))

######################################################################

#FITTING ANN WITH TANH ACTIVATION FUNCTION

library(neuralnet)
random.seed(305520)
ann.mclass<- neuralnet(as.factor(rating) ~ age + gender + member + nmovies,
data=train, hidden=3, act.fct="tanh", stepmax=1e6) 

#PLOTTING THE DIAGRAM
plot(ann.mclass)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA
pred.prob <- predict(ann.mclass, test.x)
pred.prob<- as.data.frame(pred.prob)

colnames(pred.prob)<- c(0, 0.25, 0.5, 0.75, 1)

pred.class<- apply(pred.prob, 1, function(x) colnames(pred.prob)[which.max(x)])

match<- c()
for (i in 1:length(test.y)) {
  match[i]<- ifelse(pred.class[i]==as.character(test.y[i]),1,0)
}

print(accuracy<- mean(match))

