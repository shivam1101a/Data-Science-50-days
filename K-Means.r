reddit<-read.csv('C:/Users/Shivam Singh/Desktop/movie_metadata.csv')
reddit<-na.omit(reddit) #na will remove any missing or null values
View(reddit)

facebook<-reddit[1:50,c(5,6,8,14,25,28)]#we are focusing on facebook like and the colomn number is mentioned
View((facebook))  #viewinG  1ST 50 OBSERVATIONS

#DATA CLEANING
reddit$movie_title<-sub("?","",reddit$movie_title)  #REPLACE ? WITH " " IN MOVIE TITLE
row.names(facebook)<-reddit$movie_title[1:50] #BY MOVIE NAME IN FRONT
View(facebook)

mydata<-facebook
#WSS IS WITHIN SUM OF SQUARES
wss<-(nrow(mydata)-1)*sum(apply(mydata,2,var))
for(i in 2:15) wss[i] <-sum(kmeans(mydata,centers=i)$withinss)  #CALCULATING WSS
plot(1:15,wss,type="b",xlab="Number of clusters",ylab="within groups sum of squares",las=1)
#USING PLOT THE PERFECT K WILL 2 BCZ WE SEE A SHARP DECLINE AT K=2

kmeans_model<-kmeans(mydata,2)#CHOSSING 2 CLUSTERS
summary(kmeans_model)#HIGH AND LOW RATED MOVIES DIFFERENCE
kmeans_model