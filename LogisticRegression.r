data <- read.csv(file = "E:/R-practice/Diabetes.csv",head=TRUE,sep =",")
na.omit(data)
library(caTools)
split<-sample.split(data$Outcome,SplitRatio = 0.8)
split
train<-subset(data,split==T)
test<-subset(data,split==F)
View(train)
View(test)
View(data)
#(formula=,data=,family=) is the order
mod1<-glm(Outcome~. ,train,family = "binomial")#the glm is used to make  a model in logistictic regression
summary(mod1)
#Reading the summary o the model
#Estimate the value of coefficients like b0,b1,b2
#*** represent how the significant the particlular vaiable is
#Null deviance provide the value u get if u only use intercept b0
#residual deviance when we include the independent variable
#AIC is use to remove not neccesary indpendent variable

#We use non star values
#Now lets optimize the mod1
#when we optimize the residual deviance should not inc.
#and AIC should decrease

mod2<-glm(Outcome~. - Age,train,family = "binomial")#the glm is used to make  a model in logistictic regression
summary(mod2)
#that by looking at the value of residual deviance
#and AIC age is a significant variable
mod3<-glm(Outcome~. - BloodPressure,train,family = "binomial")#the glm is used to make  a model in logistictic regression
summary(mod3)
#that by looking at the value of residual deviance
#and AIC age is a significant variable
mod4<-glm(Outcome~. - Pregnancies,train,family = "binomial")#the glm is used to make  a model in logistictic regression
summary(mod4)
#same
mod5<-glm(Outcome~. - SkinThickness,train,family = "binomial")#the glm is used to make  a model in logistictic regression
summary(mod5)
#So the insignificant part was skinThickness

res<-predict(mod5,test,type = "response")
res
#confusion matrix
table(ActualValue=test$Outcome,PredictedValue=res>0.5)
#updating the res
res<- predict(mod5,train,type="response")

dat <- data.frame(npreg=5,bp=100,age=34)

install.packages("ROCR")
library(ROCR)
#these are variables which the ROCR uses to plot the graph
#ROCRPred is used to make prediction
#ROCRPref is used to check preformance of prediction
#Tpr=truePositiveRate
ROCRPred = prediction(res,train$Outcome)
ROCRPref<-performance(ROCRPred,"tpr","fpr")
plot(ROCRPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

#comparing
table(ActualValue=test$Outcome,PredictedValue=res>0.2)
