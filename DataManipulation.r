install.packages("hflights")
library(hflights)
write.csv(hflights, "hflights.csv")

library(dplyr)
str(hflights)
View(hflights)

#select function

select(hflights,FlightNum,ArrTime,DepTime)->flight1  #flightNm is col. name
View(flight1)
select(hflights,5,6,8)->flight1  #column number
select(hflights,contains("Time"))->flight1  #col which contains time
select(hflights,Year:ArrTime)->flight1#col. from year to Arrtime
select(hflights,1:6)->flight1#first 6 col.
select(hflights,starts_with("Day"),ends_with("Time"))->flight1#all

#mutate
#bring the new column

mutate(hflights,ActualGroundTime=ActualElapsedTime-AirTime)->flight2#ActualGroundTime is new Col.
View(flight2)
mutate(flight2,AverageSpeed=Distance/AirTime *60)->flight2
mutate(flight2,TotalTaxii=TaxiIn+TaxiOut)->flight1
mutate(flight2,TimeLoss=ArrDelay+DepDelay)->flight1

 #filter
#filtering rows based on a specific condition

filter(hflights,Distance>3000)->flight3
View(flight3)
range(flight3$Distance)
filter(hflights,UniqueCarrier %in% c("OO","US","AA"))->flight3
filter(hflights,TaxiIn+TaxiOut>AirTiME)->flight3
filter(hflights,DepTime<500|ArrTime>2200)->flight3#500 is 5:00
filter(hflights,Dest=="JFK"&Cancelled==1)->flight3

#arrange
#asc or desc according to 1 particular order

arrange(hflights,DepDelay)->flight4
arrange(hflights,AirTime)->flight4
arrange(hflights,DepDelay+ArrDelay)->flight4

#summarise

summarise(hflights,min_dist=min(Distance),max_dist=max(Distance))
summarise(hflights,earliest=min(ArrDelay,na.rm=T),average=mean(ArrDelay,na.rm=T),latest=max(ArrDelay,na.rm=T),sd=sd(ArrDelay,na.rm=T))

#pipe
#selecting more col. and subsituing conditions

hflights %>% select(contains("Time")) %>% filter(AirTime>60)->flight5
hflights %>% filter(UniqueCarrier=="wN") %>% summarise(Min_Time=min(AirTime,na.rm = T))


