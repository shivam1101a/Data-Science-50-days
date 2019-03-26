library(caret)
heart<-read.csv("C:/Users/Shivam Singh/Desktop/heart1.csv",sep=',',header=FALSE)
str(heart)
head(heart)

set.seed(3033)
intrain<-createDataPartition(y=heart$V14,p=0.7,list=FALSE)
train<-heart[intrain,]
test<-heart[-intrain,]

dim(train);
dim(test);

#check for na
anyNA(heart)

summary(heart)
#factorize
train[["V14"]]=factor(train[["V14"]])

trctrl<-trainControl(method = "repeatedcv",number = 10,repeats = 3)
#repeated cross validation

#train method is essential
svm_Linear<-train(V14~.,data=train,method="svmLinear",trControl=trctrl,
                  preProcess=c("center","scale"),
                  tuneLength=10)
#svm kernal
svm_Linear

test_pred<-predict(svm_Linear,newdata = test)
test_pred

confusionMatrix(table(test_pred,test$V14))

#improving the model

grid<-expand.grid(C=c(0,0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,5))
svm_Linear_Grid <-train(V14~.,data=train,method="svmLinear",
                        trControl=trctrl,
                        preProcess=c("center","scale"),
                        tuneGrid=grid,
                        tuneLength=10)

svm_Linear_Grid
plot(svm_Linear_Grid)

test_pred_grid <- predict(svm_Linear_Grid,newdata=test)
test_pred_grid

confusionMatrix(table(test_pred_grid,test$V14))