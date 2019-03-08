house <- read.csv("C:/Users/Shivam Singh/Desktop/Average-prices-Property-Type-2018-07.csv",stringsAsFactors = FALSE)
library(ggplot2)

View(house)
#histogram

ggplot(data=house,aes(x=Flat_Average_Price))+geom_histogram()
ggplot(data=house,aes(x=Flat_Average_Price))+geom_histogram(fill="palegreen4",col="green")

#bar code
ggplot(data = house,aes(x=Flat_Average_Price))+geom_bar(fill="orange")
#scatter plot
ggplot(data=house,aes(y=Flat_Average_Price,x=Flat_Index,col=factor(Date)))+geom_point()

#box-plot
ggplot(data=house,aes(y=Flat_Average_Price,x=factor(Date),fill=factor(Date)))+geom_boxplot()


