# prac 3

iris.pca = prcomp(iris[,1:4])

# Gives how much variation (sd) (and proportion) is in each component
summary(iris.pca)

# plots the variance of each component, now you can see which component as the majority of variation
plot(iris.pca)

# Gives a summy of standard deviations and the rotation from the original axis to the new ones
iris.pca

# Gives you list of all data points in new coordinate system.. 
iris.pca$x 

# Gives you information on how the axis have been shifted for each dimension (actual principal components)
# PC1 is mainly in petal length, this is due to larger variation in petal length (scaling could be an issue)
iris.pca$rotation

# From plots below we can see with PC1 seems to show a good distinction between the different speicies, where as all the others look like a messy blob. which suggest using pc one. may not have enough info to classify based on this.
plot(iris[,1:4], pch=23, bg=c("red","green","blue")
     [unclass(iris$Species)])
pairs(iris.pca$x, pch=23, bg=c("red","green","blue")
      [unclass(iris$Species)])

# Scale tells us if we need to scale are variables, It's true or false and does a unit scale
# center tells us if the variables should be shifted to be zero centered. Alternatively the vector length equals nmber of colns supped. Either T or F
?prcomp



# Again with center =T (this is default). This should always be true because sets your plot from the new origin, which allows you to see how the data is now plotted after the transformation. If False you'd be looking at the data from a far which isn't often useful as it confuses the axis (not rotated)
iris.pca = prcomp(iris[,1:4],center = T)

pairs(iris.pca$x, pch=23, bg=c("red","green","blue")
      [unclass(iris$Species)])

# again with scale =T
# Chaning the scale affects the data quite a lot
# this is because the distance between the points change
# this now means if you want above 80% variance answered for you need
# 2 principal components rather than 1
# also the make up of these pc have changed as you would expected when stretching the dimensions
iris.pca = prcomp(iris[,1:4],center = T,scale=T)
summary(iris.pca)
plot(iris.pca)
iris.pca
pairs(iris.pca$x, pch=23, bg=c("red","green","blue")
      [unclass(iris$Species)])

# Split data up into test and train

train=rbind(iris[iris$Species == unique(iris$Species)[1],][1:25,],iris[iris$Species == unique(iris$Species)[2],][1:25,],iris[iris$Species == unique(iris$Species)[3],][1:25,])
test=rbind(iris[iris$Species == unique(iris$Species)[1],][26:50,],iris[iris$Species == unique(iris$Species)[2],][26:50,],iris[iris$Species == unique(iris$Species)[3],][26:50,])
# Alterative method
train_ind=c(1:25,51:75,101:125)
train=iris[train_ind,]
test=iris[-train_ind,]
# test PCA

iris.train.pca = prcomp(train[,1:4],center = T,scale.=T)
summary(iris.train.pca)
plot(iris.train.pca)
iris.train.pca
pairs(iris.train.pca$x, pch=23, bg=c("red","green","blue")
      [unclass(train$Species)])


# 
numComponents=2
pcaTrain <- prcomp(train[,1:4], center = TRUE,scale.
                     = TRUE)
pairs(pcaTrain$x[,1:numComponents], pch=23,
        bg=c("red","green","blue")[unclass(train$Species)])


Xhat = t(t(pcaTrain$x[,1:numComponents]
           %*%
             t(pcaTrain$rotation[,1:numComponents])) *
           pcaTrain$scale + pcaTrain$center)

pairs(Xhat, pch=23,
      bg=c("red","green","blue")
      [unclass(train$Species)])

testPCA = predict(pcaTrain, test)
pairs(testPCA, pch=23,
      bg=c("red","green","blue")
      [unclass(train$Species)])



# only two training data points 48 test.

train_ind=c(1:2,51:52,101:102)
train=iris[train_ind,]
test=iris[-train_ind,]

pcaTrain <- prcomp(train[,1:4], center = TRUE,scale.
                   = TRUE)
pairs(pcaTrain$x[,1:numComponents], pch=23,
      bg=c("red","green","blue")[unclass(train$Species)])


Xhat = t(t(pcaTrain$x[,1:numComponents]
           %*%
             t(pcaTrain$rotation[,1:numComponents])) *
           pcaTrain$scale + pcaTrain$center)

pairs(Xhat, pch=23,
      bg=c("red","green","blue")
      [unclass(train$Species)])

testPCA = predict(pcaTrain, test)
pairs(testPCA, pch=23,
      bg=c("red","green","blue")
      [unclass(train$Species)])
# Needs a larger training set!!!



### KNN 
train_ind=c(1:25,51:75,101:125)
train=iris[train_ind,]
test=iris[-train_ind,]

# make sure using transformed coord
KNN=knn(train = pcaTrain$x[,1:4],test = test[,1:2],cl=train[,5],k=5)
