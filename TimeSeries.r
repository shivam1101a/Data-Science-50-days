#Stationary
1.mean constant wrt to time
2.Variance should be equalat diff. time interval
  from the mean.
3.Variance is distance from mean
4.Co-variance should also be equal


data("AirPassengers")
class(AirPassengers)
#this tells you that the data series is in a time series format
#ts

start(AirPassengers)
#this is the start of the time series

end(AirPassengers)

frequency(AirPassengers)
#The cycle of this time series is 12months in a year

summary(AirPassengers)
#min is minimum cost
#1st quad avg. cost is given 

plot(AirPassengers)

abline(reg=lm(AirPassengers~time(AirPassengers)))
#will give the mean line 
cycle(AirPassengers)
#label

plot(aggregate(AirPassengers,FUN = mean))
#displaying on year by year trend
boxplot(AirPassengers~cycle(AirPassengers))
#for seasonal effect

#to make it close stationary
plot((log(AirPassengers)))

#i want to mean stay constant w.r.t to time
#to make it stationary
#we differentiate it to make it stationary
plot((diff(log(AirPassengers))))
#now it is staionary

#-------------------
#AR I MA
# p  d q
#AR  auto regression is seeing past value to predict own values
#I is integeration
#MA  moving average is calculated via formula in which we take differnt interval to calculate average

install.packages("tseries")
library(tseries)
adf.test(diff(log(AirPassengers)),alternative=c("stationary","explosive"),k=)

#acf is autocorrelation function
acf(AirPassengers)
#we need to keep value below the blue line 
acf(diff(log(AirPassengers))) #Detemine the value of q
#we will conisder the line before the first inverted line and stores its value in q
#line starts from zero

#partial autocorrelation function
pacf(diff(log(AirPassengers))) #determine the value of p
#we will conisder the line before the first inverted line and stores its value in p

#d is the number of times diff. is done to get the constant mean

#----------
#model
#lets fit an ARIMA model and predict the future 10 years
#     c(p,d,q) get this from graph

fit<-arima(log(AirPassengers),c(0,1,1),seasonal = list(order=c(0,1,1),period=12))

pred<-predict(fit,n.ahead=10*12)
#time*freq
pred1<-2.718^pred$pred
#2.718 is used to convert logrithm values in decimal values, it is value of e

ts.plot(AirPassengers,2.718^pred$pred,log="y",lty=c(1,3))
#dotted lines is what it is predicted for the coming years
#we have used the seasnality,irregularity,trend is taken care for 

#Testing our model

datawide<-ts(AirPassengers, frequency = 12, start = c(1949,1),end=c(1959,12))

fit<- arima(log(datawide),c(0,1,1),seasonal = list(order=c(0,1,1),period=12))

pred<-predict(fit,n.ahead=10*12)

pred1<-2.718^pred$pred

data1<-head(pred1,12)

predicted_1960 <- round(data1,digits = 0)

original_1960 <- tail(AirPassengers,12)

ts.plot(AirPassengers,2.718^pred$pred,log="y",lty=c(1,3))