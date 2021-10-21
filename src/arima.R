rm(list=ls())
setwd("/Users/apple/Desktop/Datasets")
getwd()
tsdata <- read.csv(file.choose() ,header=TRUE, sep=",")
View(tsdata)
myts   <- ts(tsdata$Total, start=c(1996, 1), end=c(2005, 4), frequency=4) 

#Plot Series 
plot(myts)



#time series  seasonal, and irregular components  
plot(decompose(myts, type = c("multiplicative")))

fit <- stl(myts, s.window="period")
fit
plot(fit)
summary(fit)
# Forecasting
# predictive accuracy by ets module for best MAPE Values
library(forecast)

fit1<-ets(myts)
accuracy(fit1$fitted, myts)  #MAPE Of 1.948
fit1
summary(fit1)

# predict next three future values

forecast(fit1, 4)
plot(forecast(fit1, 4))

# Automated forecasting using an ARIMA model
fit2 <- auto.arima(myts)

# predictive accuracy
library(forecast)
accuracy(fit2)

# predict next 4 observations
library(forecast)
forecast(fit2, 4)
plot(forecast(fit2, 4))

