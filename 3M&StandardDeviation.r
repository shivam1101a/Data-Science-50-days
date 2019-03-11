 set.seed(1)
 data = runif(20, 1, 10)
 hist(data, bins=10, range=c(0,10), edgecolor='black')
 mean = mean(data)
 print(mean)
 median = median(data)
 print(median(data))
 
 #Mode function
 Mode <- function(x){
   print(x)
   ux <- unique(x)
   ux[which.max(tabulate(match(x,ux)))]
 }
 result <- Mode(data)
 print(data)
 cat("Mode = {}", result)

 #Standard deviation
 variance=var(data)
 standardDeviation=sqrt(var(data))