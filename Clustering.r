iris <- read.csv("C:/Users/Shivam Singh/Desktop/iris.csv",stringsAsFactors = FALSE)
View(iris)
iris[,-5]->iris4
kmeans(iris4,3)->k1
cbind(iris4,k1$cluster)->iris4
table(iris4$`k1$cluster`)

cbind(iris4,iris$species)->iris4
View(iris4)