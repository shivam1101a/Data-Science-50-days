cancerData <- read.csv("C:/Users/Shivam Singh/Desktop/BreastCancer.csv",stringsAsFactors = FALSE)
str(cancerData)
cancerData$id<-NULL   #AS IS NOT NEEDED TO PREDICT
cancerData$X<-NULL   #AS IS NOT NEEDED TO PREDICT
#cancerData$X <- as.numeric(cancerData$X)
#THIS will convert into char into numeric, not int
#cancerData <-cancerData[complete.cases(cancerData),]  #This will identitfy rows without data
str(cancerData)
#Transform classes of M and B in malignant and benial
#cancerData$diagnosis <- factor(ifelse(cancerData$diagnosis=='M',"malignant","benign"))

##BUILDING A MODEL
#Data splicing  #training set 
trainingSet<-cancerData[1:528,2:31]
testSet<-cancerData[529:569,2:31]#no OUTPUT SHOULD BE STORED

trainingOut<-cancerData[1:528,1]
testOut<-cancerData[529:569,1]

#installed.packages("class")
library(class)
predictions<-knn(train = trainingSet,cl=trainingOut,k=23,test = testSet)
#23 is used bcz 23 is near root of 528

#Display predictions
predictions

#Model evaluation
table(testOut,predictions)

#Finding accuracy
actual_preds<-data.frame(cbind(actuals=testOut,predicted=predictions))
correlation_accuracy<-cor(actual_preds) #Storing the correlation
head(actual_preds)
