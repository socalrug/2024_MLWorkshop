pneumonia.data<- read.csv(file = here::here("DATA_SETS/pneumonia_data.csv"),
                          header=TRUE, sep=",")

pneumonia.data$pneumonia<- ifelse(pneumonia.data$pneumonia=="yes",1,0)
pneumonia.data$gender<- ifelse(pneumonia.data$gender=='M',1,0)
pneumonia.data$tobacco_use<- ifelse(pneumonia.data$tobacco_use=='yes',1,0)                                         

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(966452)
sample <- sample(c(TRUE, FALSE), nrow(pneumonia.data), replace=TRUE, prob=c(0.8,0.2))
train<- pneumonia.data[sample,]
test<- pneumonia.data[!sample,]

train.x<- data.matrix(train[-5])
train.y<- data.matrix(train[5])
test.x<- data.matrix(test[-5])
test.y<- data.matrix(test[5])

library(e1071)

#FITTING SVM WITH LINEAR KERNEL
svm.class<- svm(as.factor(pneumonia) ~ gender + age + tobacco_use + PM2_5, data=train, 
kernel="linear")

#computing prediction accuracy for testing data
pred.y<-as.numeric(predict(svm.class, test.x))-1

match<- c()
for (i in 1:length(pred.y))
  match[i]<- ifelse(test.y[i]==pred.y[i], 1,0)
print(paste("accuracy=", round(mean(match), digits=4)))

#FITTING SVM WITH POLYNOMIAL KERNEL
svm.class<- svm(as.factor(pneumonia) ~ gender + age + tobacco_use + PM2_5, 
data=train, kernel="polynomial")

#computing prediction accuracy for testing data
pred.y<- as.numeric(predict(svm.class, test.x))-1

for (i in 1:length(pred.y))
  match[i]<- ifelse(test.y[i]==pred.y[i], 1,0)
print(paste("accuracy=", round(mean(match), digits=4)))

#FITTING SVM WITH RADIAL KERNEL
svm.class<- svm(as.factor(pneumonia) ~ gender + age + tobacco_use + PM2_5, 
data=train, kernel="radial")

#computing prediction accuracy for testing data
pred.y<- as.numeric(predict(svm.class, test.x))-1

for (i in 1:length(pred.y))
  match[i]<- ifelse(test.y[i]==pred.y[i], 1,0)
print(paste("accuracy=", round(mean(match), digits=4)))

#FITTING SVM WITH SIGMOID KERNEL
svm.class<- svm(as.factor(pneumonia) ~ gender + age + tobacco_use + PM2_5, 
data=train, kernel="sigmoid")

#computing prediction accuracy for testing data
pred.y<- as.numeric(predict(svm.class, test.x))-1

for (i in 1:length(pred.y))
  match[i]<- ifelse(test.y[i]==pred.y[i], 1,0)
print(paste("accuracy=", round(mean(match), digits=4)))
