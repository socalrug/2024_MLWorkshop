pneumonia.data<- read.csv(file = here::here("DATA_SETS/pneumonia_data.csv"),
                          header=TRUE, sep=",")

pneumonia.data$pneumonia<- ifelse(pneumonia.data$pneumonia=="yes",1,0)
pneumonia.data$gender<- ifelse(pneumonia.data$gender=='M',1,0)
pneumonia.data$tobacco_use<- ifelse(pneumonia.data$tobacco_use=='yes',1,0)                                         

#SCALING VARIABLES TO FALL IN [0,1]
library(dplyr)

scale01 <- function(x){
  (x-min(x))/(max(x)-min(x))
}

pneumonia.data<- pneumonia.data %>% mutate_all(scale01)

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(503548)
sample <- sample(c(TRUE, FALSE), nrow(pneumonia.data), replace=TRUE, prob=c(0.8,0.2))
train<- pneumonia.data[sample,]
test<- pneumonia.data[!sample,]

train.x<- data.matrix(train[-5])
train.y<- data.matrix(train[5])
test.x<- data.matrix(test[-5])
test.y<- data.matrix(test[5])

################################################################################

#FITTING ANN WITH LOGISTIC ACTIVATION FUNCTION AND ONE LAYER WITH THREE NEURONS
library(neuralnet)
ann.class<- neuralnet(as.factor(pneumonia) ~ gender + age + tobacco_use + PM2_5, 
data=train, hidden=3, act.fct="logistic")

#PLOTTING THE DIAGRAM
plot(ann.class)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA
pred.prob<- predict(ann.class, test.x)[,1]

match<- c()
for (i in 1:length(test.y)){
  pred.y[i]<- ifelse(pred.prob[i]>0.5,1,0)
  match[i]<- ifelse(test.y[i]==pred.y[i],1,0)
}

print(paste("accuracy=", round(mean(match), digits=4)))

################################################################################

#FITTING ANN WITH TANH ACTIVATION FUNCTION AND ONE LAYER WITH THREE NEURONS
library(neuralnet)
ann.class<- neuralnet(as.factor(pneumonia) ~ gender + age + tobacco_use + PM2_5, 
data=train, hidden=3, act.fct="tanh", stepmax=1e6)

#PLOTTING THE DIAGRAM
plot(ann.class)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA
pred.prob<- predict(ann.class, test.x)[,1]

match<- c()
for (i in 1:length(test.y)){
  pred.y[i]<- ifelse(pred.prob[i]>0.5,1,0)
  match[i]<- ifelse(test.y[i]==pred.y[i],1,0)
}

print(paste("accuracy=", round(mean(match), digits=4)))

