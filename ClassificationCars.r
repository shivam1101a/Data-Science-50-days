cars<-read.csv("C:/Users/Shivam Singh/Desktop/Cars93.csv")
View(cars)
library(dplyr)
#removing 1st coloumn
cars %>% select(-1)->cars
View(cars)

cars1<-cars[1:98,c(1,2,3,5,11,16,18)]
View(cars1)

#Spliting the dataset
library(caTools)
sample.split(cars1$Man.trans.avail,SplitRatio = 0.65)->split_val
subset(cars1,split_val==T)->train
subset(cars1,split_val==F)->test
//train<-cars1[1:70,1:6]
//test<-cars1[71:98,1:6]

library(rpart)
rpart(Man.trans.avail~. ,data=train)->modelClass
predict(modelClass,test,type="class")->resultClass
table(test$Man.trans.avail,resultClass)#confusion matrix


