pneumonia.data<- read.csv(file = here::here("DATA_SETS/pneumonia_data.csv"),
                          header=TRUE, sep=",")

#ASSIGNING NUMERIC VALUES TO CATEGORICAL VARIABLES
pneumonia.data$pneumonia<- ifelse(pneumonia.data$pneumonia=="yes",1,0)
pneumonia.data$gender<- ifelse(pneumonia.data$gender=='M',1,0)
pneumonia.data$tobacco_use<- ifelse(pneumonia.data$tobacco_use=='yes',1,0) 
#all variables have to be numeric and part of one data set

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(1012312)
sample <- sample(c(TRUE, FALSE), nrow(pneumonia.data), 
replace=TRUE, prob=c(0.8,0.2))
train<- pneumonia.data[sample,]
test<- pneumonia.data[!sample,]

test.x<- data.matrix(test[-5])
test.y<- data.matrix(test[5])

#FITTING NAIVE BAYES BINARY CLASSIFIER
library(e1071)
nb.class<- naiveBayes(as.factor(pneumonia) ~ gender
+ age + tobacco_use + PM2_5, data=train)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA
pred.y<- as.numeric(predict(nb.class, test.x))-1

match<- c()
for (i in 1:length(pred.y))
  match[i]<- ifelse(test.y[i]==pred.y[i], 1, 0)
print(paste('accuracy=', round(mean(match)*100, digits=2),'%'))



