diabetes<-read.csv("C:/Users/Shivam Singh/Desktop/Diabetes.csv")
View(diabetes)

#instead of using library caTools and splitting the data,we can split data by
set.seed(3)
id<-sample(2,nrow(diabetes),prob=c(0.7,0.3),replace= TRUE)
train<-diabetes[id==1,]
test<-diabetes[id==2,]

#Building decision tree
library(rpart)

colnames(diabetes)

model<-rpart(Outcome~. ,data=train)#rpart does not use entropy concept of decision tree

model

plot(model,margin = 0.1)

text(model,use.n=TRUE,pretty=TRUE,cex=0.8)

predict(model,test)->pred_diabet
pred_diabet


table(pred_diabet,test$Outcome)
library(caret)

confusionMatrix(table(pred_diabet,test$Outcome))