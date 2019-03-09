install.packages("caTools")
mtcars <- read.csv("C:/Users/Shivam Singh/Desktop/mtcars.csv")
View(mtcars)
library(caTools)
sample.split(mtcars$mpg,SplitRatio = 0.65)->mysplit
train<-subset(mtcars,mysplit==T)
test<-subset(mtcars,mysplit==F)

#lm means linear model
#~ symbol is tilde
#the value on the left of ~ is dependent variable
#the value on the right of ~ is independent variables
#.  means how does mpg varies with all other col. of the dataset
#mod means model

lm(mpg~.,data=train)->mod1
#nrow(train)


predict(mod1,test)->result
View(result)

#Combining old and new data set and storing the result in new data set

cbind(actual=test$mpg,predicted=result)->final
as.data.frame(final)->final
cbind(final,error=final$actual-final$predicted)->final
View(final)
rmse1<-sqrt(mean((final$error)^2))
rmse1
