tsla.data<- read.csv(file = here::here("DATA_SETS/TSLAStockPrices.csv"),
                     header=TRUE, sep=",")

library(changepoint)
ansmean<- cpt.mean(tsla.data$Price,penalty="AIC",method="BinSeg",Q=5)
plot(ansmean,cpt.col="red", cpt.width=3, col="blue", ylab="Daily Closing Price", 
main="Change Point Detection for Change in Mean")
print(ansmean)

ansvar<- cpt.var(tsla.data$Price,penalty="AIC",method="BinSeg",Q=3)
plot(ansvar,cpt.col="red", cpt.width=3, col="blue", ylab="Daily Closing Price", 
main="Change Point Detection for Change in Variance")
print(ansvar)

ansmeanvar=cpt.meanvar(tsla.data$Price,penalty="AIC",method="BinSeg",Q=5)
plot(ansmeanvar,cpt.col="red",cpt.width=3, col="blue", ylab="Daily Closing Price", main="Change Point 
Detection for Change in Mean and Variance")
print(ansmeanvar)
