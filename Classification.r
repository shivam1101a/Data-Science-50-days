census_data<-read.csv("C:/Users/Shivam Singh/Desktop/2010_Census_Populations_by_Zip_Code.csv")
View(census_data)
library(rpart)
library(caTools)
sample.split(census_data$Median.Age,SplitRatio = 0.65)->mysplit
train<-subset(census_data,mysplit==T)
test<-subset(census_data,mysplit==F)
View(test)
rpart(Median.Age~. ,data=train,method = "class")->mod_classify
predict(mod_classify,test,type="class")->result
View(result)
install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)
confusionMatrix(result,test$Median.Age)
confusionMatrix(
  factor(result, levels = 1:315),
  factor(test$Median.Age, levels = 1:315)
)

