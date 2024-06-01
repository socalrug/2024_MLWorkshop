movie.data<- read.csv(file = here::here("DATA_SETS/movie_data.csv"), 
                                        header=TRUE, sep=",")
            
movie.data$gender<- ifelse(movie.data$gender=='M',1,0)
movie.data$member<- ifelse(movie.data$member=='yes',1,0)
#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(444625)
sample <- sample(c(TRUE, FALSE), nrow(movie.data), replace=TRUE, prob=c(0.8,0.2))
train<- movie.data[sample,]
test<- movie.data[!sample,]

train.x<- data.matrix(train[-5])
train.y<- data.matrix(train[5])
test.x<- data.matrix(test[-5])
test.y<- data.matrix(test[5])

library(e1071)

#FITTING SVM WITH LINEAR KERNEL
svm.multiclass<- svm(as.factor(rating) ~ age + gender + member + nmovies,
data=train, kernel="linear")

#computing prediction accuracy for testing data
pred.y<- as.numeric(predict(svm.multiclass, test.x))

print(paste("accuracy=", round(mean(test.y==pred.y), digits=4)))

#################################################################
#FITTING SVM WITH POLYNOMIAL KERNEL
svm.multiclass<- svm(as.factor(rating) ~ age + gender + member + nmovies,
data=train, kernel="polynomial")

#computing prediction accuracy for testing data
pred.y<- as.numeric(predict(svm.multiclass, test.x))

print(paste("accuracy=", round(mean(test.y==pred.y), digits=4)))

#################################################################
#FITTING SVM WITH RADIAL KERNEL
svm.multiclass<- svm(as.factor(rating) ~ age + gender + member + nmovies,
data=train, kernel="radial")

#computing prediction accuracy for testing data
pred.y<- as.numeric(predict(svm.multiclass, test.x))

print(paste("accuracy=", round(mean(test.y==pred.y), digits=4)))

#################################################################
#FITTING SVM WITH SIGMOID KERNEL
svm.multiclass<- svm(as.factor(rating) ~ age + gender + member + nmovies,
data=train, kernel="sigmoid")

#computing prediction accuracy for testing data
pred.y<- as.numeric(predict(svm.multiclass, test.x))

print(paste("accuracy=", round(mean(test.y==pred.y),digits=4)))
