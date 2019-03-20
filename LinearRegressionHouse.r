#Load the data
library(MASS)#it contains data set boston
data("Boston")
View(Boston)
na.omit(Boston)


#For data description
?Boston

#Split
set.seed(2)
library(caTools) 
split<-sample.split(Boston$medv,SplitRatio = 0.7)
split

train<-subset(Boston,split=="T")
test<-subset(Boston,split=="F")

#To view the correlation of variable
plot(Boston$crim,Boston$medv,cev= 0.5,xlab = "Crime rate",ylab="Price")
cr<-cor(Boston)#corrletion of boston

#creating scatterplt matrix
attach(Boston)
library(lattice)
splom(~Boston[c(1:6,14)],groups=NULL,data=Boston,axis.line.tck=0,axis.text.alpha=0)
splom(~Boston[c(7,14)],groups=NULL,data=Boston,axis.line.tck=0,axis.text.alpha=0)

#studying rm and medv
plot(rm,medv)
abline(lm(medv~rm),col="red") #regression fit line

#we can use corplot to visualize
install.packages("corrplot")
library(corrplot)

corrplot(cr,type="lower")
corrplot(cr,method = "number")

#which are near to zero are negatively correlated

#finding multicollinearity
library(caret)

#to exclude medv(output)
Boston_a =subset(Boston,select = -c(medv))
numericData <- Boston_a[sapply(Boston_a,is.numeric)]
descrCor<-cor(numericData)

#vif
install.packages("car")
library(car)
model<-lm(medv~.,data=train)
vif(model)

#now to create the model we will apply column
model<-lm(medv~.,data=train)
#model<-lm(medv~ crim+zn+....,data=train)

summary(model)

#model creation after removing tax
model<-lm(medv~ crim+zn+indus+chas+rm+rad+age+dis+rad+tax
          +ptratio+black+lstat,data = train)
summary(model)

#model creation after removing indus and age
model<-lm(medv~ crim+zn+chas+rm+rad+dis
          +ptratio+black+lstat,data = train)
summary(model)

#Now we can use this model to predict
predic<-predict(model,test)
predic

#To compare predicted value and actual value,we can use plots
plot(test$medv,type = "l",lty=1.8,col="green")
lines(predic,type = "l",col="blue")

#Now we can use this model to predict the output of sample dataset
predic<-predict(model,sample_data)
predic