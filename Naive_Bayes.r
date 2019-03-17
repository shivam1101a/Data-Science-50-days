#this model is made wrong try another model

empdata<-read.csv("C:/Users/Shivam Singh/Desktop/1000 Records.csv")
colnames(empdata)
View(empdata)

#Label
Sal<-ifelse(empdata$Salary>50000,"High","Low")
empdata<-data.frame(empdata,Sal)
empdata<-empdata[1:1000,-33]
empdata<-empdata[1:1000,-34]
empdata<-empdata[1:1000,-35]
empdata<-empdata[1:1000,-21]
empdata<-empdata[1:1000,-32]
View(empdata)

#split
set.seed(2)
id<-sample(2,nrow(empdata),prob=c(0.7,0.3),replace = TRUE)
train<-empdata[id==1,]
test<-empdata[id==2,]

#Naive bayes

library(e1071)
library(caret)

colnames(train)                  
#emp_nb<-naiveBayes(Sal.3~.,train)
emp_nb<-naiveBayes(Sal.3 ~ Age.in.Yrs.+Age.in.Company..Years.+County,train)#we seleted only the column that we required to use
emp_nb

preee3<-predict(emp_nb,test)
confusionMatrix(table(preee3,test$Sal.3))


#Kappa=(totalAccuracy-randomAccuracy)/(1-randomAccuracy)