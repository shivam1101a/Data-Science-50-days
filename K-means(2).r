rm(list = ls(all = TRUE))
movie_metadata<-read.csv("E:/R-practice/movie_metadata.csv")
View((movie_metadata))
dim(movie_metadata)#Dimensions

movie<-data.matrix(movie_metadata)#To perform certain operation
movie<-na.omit(movie)#remove the row with NA
sample<-movie[sample(nrow(movie),500),]#Random 500 rows
View(sample)

sample_short<-sample[,c(9,23)]#col. number
View(sample_short)
sample_matrix<-data.matrix(sample_short)

wss<-(nrow(sample_matrix)-1)*sum(apply(sample_matrix,2,var))#Using variance with apply
for (i in 2:15) wss[i]<-sum(kmeans(sample_matrix,centers = i)$withinss)#k-means clustering from k=1 to 15 withinss
plot(1:15,wss, type = "b",xlab="Number of Clusters",ylab = "within sum of Squares")
