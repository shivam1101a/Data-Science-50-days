diabetes<-read.csv("E:/R-practice/Diabetes.csv")
View(diabetes)

set.seed(2)
id<-sample(2,nrow(diabetes),prob = c(0.7,0.3),replace = T)
train<-diabetes[id==1,]
test<-diabetes[id==2,]

#random forest
install.packages("randomForest")
library(randomForest)

diabetes$Outcome<-as.factor(diabetes$Outcome)
train$Outcome<-as.factor(train$Outcome)
#The number of variable available for spliting at tree node
#stepFactor is uesd to inc. and reach the level the where the number of predicted variables will be optimized
#improve is used to escape from infinte loop 
bestmtry <- tuneRF(train,train$Outcome,stepFactor = 1.2,improve = 0.01,trace=T,plot = T)

diabet_forest<-randomForest(Outcome~.,data=train)
diabet_forest

importance(diabet_forest)
varImpPlot(diabet_forest)

#prediction
pred1<-predict(diabet_forest,newdata = test,type="class")
pred1

library(caret)
confusionMatrix(table(pred1,test$Outcome))