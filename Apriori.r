install.packages("arules") #for algorithm
library(arules)

datagr <-read.transactions("C:/Users/Shivam Singh/Desktop/Groceries.csv", sep=",")
datagr  #sparse format

summary(datagr)

inspect(datagr)
inspect(datagr[1:3])
inspect(datagr[1001:1003])

itemFrequency(datagr[,1])   #will give support of 1st item
itemFrequency(datagr[,1:6]) #will give support for 1-6 items
itemFrequencyPlot(datagr,support=0.08)  #items that are in 8% of transaction
itemFrequencyPlot(datagr,topN=5)  #top 5 items purchased

#Apriori
#applying model
model1<-apriori(datagr) #Default Support=0.1 confi=0.8
model1
summary(model1)

#setting support and confidence
model2<-apriori(datagr,parameter = list(support=0.01,confidence=0.5))
model2
summary(model2)

inspect(model2)
inspect(model2[1:3])

#soring by lift
inspect(sort(model2,by="lift")[1:5])

#rule for specific items
#trying to find out if people buy yogurt what else are they interseted in
model3<-apriori(datagr,parameter = list(support=0.01,confidence=0.2),appearance=list(default="rhs",lhs=c("whole milk","yogurt")))
summary(model3)
inspect(model3)

#visualising model
install.packages("arulesViz")
library(arulesViz)
plot(model3)