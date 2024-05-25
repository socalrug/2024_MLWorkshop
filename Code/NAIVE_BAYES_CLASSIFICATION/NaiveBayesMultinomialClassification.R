movie.data<- read.csv(file = here::here("DATA_SETS/movie_data.csv"), 
                      header=TRUE, sep=",")
            
movie.data$gender<- ifelse(movie.data$gender=='M',1,0)
movie.data$member<- ifelse(movie.data$member=='yes',1,0)

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(444625)
sample <- sample(c(TRUE, FALSE), nrow(movie.data), replace=TRUE, prob=c(0.8,0.2))
train<- movie.data[sample,]
test<- movie.data[!sample,]

test.x<- data.matrix(test[-5])
test.y<- data.matrix(test[5])

#FITTING NAIVE BAYES BINARY CLASSIFIER
library(e1071)
nb.multiclass<- naiveBayes(as.factor(rating) ~ age + gender + member	
+ nmovies, data=train)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA
pred.y<- as.numeric(predict(nb.multiclass, test.x))

print(paste('accuracy=', round((1-mean(test.y!=pred.y))*100, digits=2), '%'))

