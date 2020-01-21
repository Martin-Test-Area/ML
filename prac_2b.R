# prac_2b

X=iris[,1:2]
Y=iris[,5]

?kmeans

result <- kmeans(X,X[1:3,],iter.max=1,algorithm="Lloyd")
result$centers
result$cluster
plot(X,bg=c("red","green","blue")[unclass(result$cluster)],pch=23)
points(x=iris$Sepal.Length,iris$Sepal.Width,col=c("red","green","blue")[unclass(iris$Species)],pch=23)

# change initialization
init=data.frame(Sepal.Length=c(5,6,7),
                Sepal.Width=c(3.5,2.5,2.5))
result <- kmeans(X,init,iter.max=1,algorithm="Lloyd")
result$centers
result$cluster
plot(X,bg=c("red","green","blue")[unclass(result$cluster)],pch=23)
points(x=iris$Sepal.Length,iris$Sepal.Width,col=c("red","green","blue")[unclass(iris$Species)],pch=23)




# 4 repeat running code to see the how it changes with each iteration
result <- kmeans(X,result$centers,iter.max=1,algorithm="Lloyd")
result$centers
result$cluster
plot(X,col=result$cluster)

#5. We can let the algorithm itself choose the initial values, and only specify the number of clusters. Run this code several times and observe the differences.
numClusters <- 3
result <- kmeans(X,numClusters,nstart=1,iter.max=1,algorithm="Lloyd")
result$centers
result$cluster
plot(X,col=result$cluster)

# change nstart=1000. This changes the number of random sets to be choosen to 1000. E.g. start iterations after finding a good place to start. (I think sets the starting point at the average?? need to check)
numClusters <- 3
result <- kmeans(X,numClusters,nstart=1000,iter.max=1,algorithm="Lloyd")
result$centers
result$cluster
plot(X,col=result$cluster)

# 7 try default
result = kmeans(X,numClusters)
plot(X,col=result$cluster)


# how many clusters do we need for a clean devide... Well this is unsupervised not supervised.
# But more clusters the more messy it seems. If not 6. if supervised then number of clusters=number of classes unless bimodal?
# Clean is subjective...


# Crash and burned from here
par(mfrow=c(3,3))
numClusters=seq(1:9)
for(i in 1:9){
result = kmeans(X,numClusters[i])
plot(X,col=result$cluster)}
par(mfrow=c(1,1))

result = kmeans(X,2)
plot(X,col=result$cluster)
## Labbleed and supervised. Actual data is messy... 
# Real
plot(iris$Sepal.Length,iris$Sepal.Width,col=iris$Species)

library(dplyr)
set.seed(100)
data.train=sample_frac(iris,0.5)
iris_index <- as.numeric(rownames(data.train))
data.test <- iris[-iris_index,]
labels3=data.train[,5]
results3=knn(train = data.train[,1:4],test = data.test[,1:4],cl=labels3,k=3)
results3==data.test[,5]
plot(x=data.test$Sepal.Length,y=data.test$Sepal.Width,col=results3)
