pneumonia.data<- read.csv(file = here::here("DATA_SETS/pneumonia_data.csv"),
                          header=TRUE, sep=",")

#SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS 
set.seed(283605)
sample <- sample(c(TRUE, FALSE), nrow(pneumonia.data), 
replace=TRUE, prob=c(0.8,0.2))
train<- pneumonia.data[sample,]
test<- pneumonia.data[!sample,]

#FITTING PRUNED BINARY TREE WITH GINI SPLITTING 
library(rpart)
tree.gini<- rpart(pneumonia ~ age + gender + tobacco_use + PM2_5, 
data=train, method="class", parms=list(split="Gini"), maxdepth=4)

#PLOTTING THE DECISION TREE
library(rpart.plot)
rpart.plot(tree.gini, type=3)

#COMPUTING PREDICTION ACCURACY FOR TESTING DATA 
pred.prob<- predict(tree.gini, test)

match<- c()
for (i in 1:nrow(pred.prob))
  match[i]<- ifelse((pred.prob[i,2] > 0.5 & 
    test[i,5]=="yes"|pred.prob[i,2] <= 0.5 & 
        test[i,5]=="no"), 1,0) #Overall accuracy

print(paste("accuracy=", round(mean(match), digits=4)))

