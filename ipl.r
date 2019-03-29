ipl1<-read.csv("E:/R-practice/iplallseasons_refined.csv")
View(ipl1)

ipl2<-ipl1[1:56,c(3,5,6,7,8,9,10,11,12)]
View(ipl2)

#cars1<-cars[1:98,c(1,2,3,5,11,16,18)]
#View(cars1)

#select function

select(hflights,FlightNum,ArrTime,DepTime)->flight1  #flightNm is col. name
View(flight1)
select(hflights,5,6,8)->flight1  #column number
select(hflights,contains("Time"))->flight1  #col which contains time
select(hflights,Year:ArrTime)->flight1#col. from year to Arrtime
select(hflights,1:6)->flight1#first 6 col.
select(hflights,starts_with("Day"),ends_with("Time"))->flight1#all

#
library(dplyr)
mutate(ipl2,score1=Team1_score)->ipl2

#mutate
#bring the new column

mutate(hflights,ActualGroundTime=ActualElapsedTime-AirTime)->flight2#ActualGroundTime is new Col.
View(flight2)
mutate(flight2,AverageSpeed=Distance/AirTime *60)->flight2
mutate(flight2,TotalTaxii=TaxiIn+TaxiOut)->flight1
mutate(flight2,TimeLoss=ArrDelay+DepDelay)->flight1
#

#filter
#filtering rows based on a specific condition

filter(hflights,Distance>3000)->flight3
View(flight3)
range(flight3$Distance)
filter(hflights,UniqueCarrier %in% c("OO","US","AA"))->flight3
filter(hflights,TaxiIn+TaxiOut>AirTiME)->flight3
filter(hflights,DepTime<500|ArrTime>2200)->flight3#500 is 5:00
filter(hflights,Dest=="JFK"&Cancelled==1)->flight3
#

#
movie<-data.matrix(movie_metadata)#To perform certain operation
movie<-na.omit(movie)#remove the row with NA
sample<-movie[sample(nrow(movie),500),]#Random 500 rows
View(sample)

sample_short<-sample[,c(9,23)]#col. number
View(sample_short)
sample_matrix<-data.matrix(sample_short)
#

#
facebook<-reddit[1:50,c(5,6,8,14,25,28)]#we are focusing on facebook like and the colomn number is mentioned
View((facebook))  #viewinG  1ST 50 OBSERVATIONS


ipl2$Winning_margin[which(is.na(ipl2$Winning_margin))]
ipl2$Winning_margin<-sub("NA,"draw",ipl2$Winning_margin)


#DATA CLEANING
reddit$movie_title<-sub("?","",reddit$movie_title)  #REPLACE ? WITH " " IN MOVIE TITLE
row.names(facebook)<-reddit$movie_title[1:50] #BY MOVIE NAME IN FRONT
View(facebook)
#

#
#renaming columns
colnames(pokemon)[2]<-"Primary_type"
colnames(pokemon)[3]<-"Secondary_type"

#as.factor(pokemon$isLegendary)->pokemon$isLegendary

#
#seleting Grass pokemon
library(dplyr)
pokemon %>% filter(Primary_type=="Grass")->Grass_pokemon
Grass_pokemon %>% filter(Secondary_type=="Poison")->Grass_Poison_pokemon
range(Grass_Poison_pokemon$Speed)
Grass_Poison_pokemon %>% filter(Speed==90)->My_Grass_pokemon
View(My_Grass_pokemon)
#

#
