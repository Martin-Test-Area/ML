# practical 2

View(iris3)

# first 50 rows, from list 1, setosa
iris3[1:50,,1]

# prints the petal lengths in vector or species
iris$Petal.Length
iris$Species

# bind the first 25 rows/results for the first 3 species of flowers
rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])

# plots the petal length against observatio number
plot(iris$Petal.Length, main="Iris Data")
#plots petal length against width (x=length,y=width)
plot(iris$Petal.Length, iris$Petal.Width, main="Iris Data",bg="red")
#creates array of plots of all the different variable
plot(iris, main="Iris Data",bg=c("red","green","blue")[unclass(iris$Species)],pch=23)


#changes the shape of the point
pch=23 # use the help to search what ‘pch’ is
# changes color of point, need pch element for colour to work
bg="red"
# same again... but different colour for different speices
bg=c("red","green","blue")[unclass(iris$Species)]

# vector of colours..
c("red","green","blue")
# assigns number to factor. based on factor level
unclass(iris$Species)
# selects 2nd element in vector
c("red","green","blue")[2]
# assigns colour to species. This is based on factor level and position in vector
c("red","green","blue")[unclass(iris$Species)]


library(class)
data <- iris[,1:4]
labels <- iris[,5]
result <- knn(data, data, labels,k=3)
result

# change K by ading k=1... into element
knn(data,data,labels,k=3)
# True if they match, False if they do not. check real data with knn data set.
result==labels

# Plot showing if data classified correctly
iris2=iris
iris2$mis= as.factor(result==labels)
plot(iris2, main="Iris Data",bg=c("red","green")[unclass(iris2$mis)],pch=23)

# Split data non-random
training=iris[1:75,1:4]
labels2=labels[1:75]
real=iris[76:150,1:4]
results2=knn(train = training,test = real,cl=labels2,k=3)
results2==labels[76:150]



# Split data randomisd
library(dplyr)
set.seed(100)
data.train=sample_frac(iris,0.5)
iris_index <- as.numeric(rownames(data.train))
data.test <- iris[-iris_index,]
labels3=data.train[,5]
results3=knn(train = data.train[,1:4],test = data.test[,1:4],cl=labels3,k=3)
results3==data.test[,5]
