{r,message=FALSE,warning=FALSE,results='hide'}

install.packages("tidyr") #cleaning data set
install.packages("knitr") #dynamic report generation
install.packages("rmarkdown") #markdown language
library(dplyr)  #manipulating data  
library(ggplot2)  #r ploting
handsOn<-read.csv("C:/Users/Shivam Singh/Desktop/Online Retail.csv")
library(tidyr)
library(knitr)
library(rmarkdown)

#lets load and examine the dataset
df_data<-handsOn
glimpse(df_data)

#DATA CLEANING
#IN THE DATA CLEANING PART WE DELETE ALL THE NEGATIVE 
#QUANTITIES AND NA CUSTOMER IDs.
df_data<-df_data %>%
  mutate(Quantity=replace(Quantity,Quantity<=0,NA),
         UnitPrice=replace(UnitPrice,UnitPrice<=0,NA))

df_data<-df_data %>%
  drop_na() #droping all Non-applicable values according to the condition

#RECODING VARIABLE
df_data<-df_data %>%
  mutate(InvoiceNo=as.factor(InvoiceNo),StockCode=as.factor(StockCode),
         InvoiceDate=as.Date(InvoiceDate,'%m/%d/%Y %H:%M'),CustomerID=as.factor(CustomerID),
         Country=as.factor(Country))

df_data<-df_data %>%
  mutate(total_dolar=Quantity*UnitPrice)  #CALCULATION OF TOTAL_DOLLAR


glimpse(df_data)

#CALCULATING RFM
df_RFM <- df_data %>%
  group_by(CustomerID) %>%
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequenci=n_distinct(InvoiceNo),monitery=sum(total_dolar)/n_distinct(InvoiceNo))  #CALCUALTING RFM

summary(df_RFM)

kable(head(df_RFM)) #PROPER TABLE IN DECENDING ORDER

#RECENCY
hist(df_RFM$recency)

#FREQUENCY
hist(df_RFM$frequenci, breaks = 50)

#MONETARY
hist(df_RFM$monitery,breaks = 50)

#LOG SCALE TO NORMALIZE
df_RFM$monitery<-log(df_RFM$monitery)
hist(df_RFM$monitery)

#CLUSTERING
df_RFM2<-df_RFM
row.names(df_RFM2)<-df_RFM2$CustomerID
df_RFM2$CustomerID <-NULL

df_RFM2<-scale(df_RFM2)
summary(df_RFM2)

d<-dist(df_RFM2)
C<-hclust(d,method='ward.D2') #DENDOGRAM

plot(c)

#Cutree will be used to cut dendogram
members<-cutree(c,k=8)  #DOUBT

members[1:5]
table(members)

aggregate(df_RFM[,2:4],by=list(members),mean) 