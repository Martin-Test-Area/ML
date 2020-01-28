# prac 4

library(nnet)
# XOR data (blue and red points)
x1=c(1, 1, 0.9, 1, 1, 0.9, 0, 0, 0.1, 0, 0, 0.1 )
x2=c(1, 0.9, 1, 0, 0.1, 0, 1, 0.9, 1, 0, 0.1, 0 )
xData = cbind(x1,x2)
yData=c(0,0,0,1,1,1,1,1,1,0,0,0)
# hidden neuron 1 (orange plot)
n1Scaling <- 100
n1WeightX1 <- -1*n1Scaling
n1WeightX2 <- 1*n1Scaling
n1Offset <- -0.5*n1Scaling
# hidden neuron 2 (green plot)
n2Scaling <- 100
n2WeightX1 <- 1*n2Scaling
n2WeightX2 <- -1*n2Scaling
n2Offset <- -0.5*n2Scaling
# hidden neuron 2 (black plot)
oScaling <- 10
oWeightH1 <- 1*oScaling
oWeightH2 <- 1*oScaling
oOffset <- -0.5*oScaling


# bias,x1,x2 -> first hidden, then second hidden; bias,h1,h2 -> output
W <- c(n1Offset,n1WeightX1,n1WeightX2,n2Offset,n2WeightX1,n2WeightX2,oOffset,oWeightH1,oWeightH2)
nn <- nnet(matrix(c(0,0),nrow=1, ncol=2), matrix(c(0), nrow=1, ncol=1), size = 2, maxit = 1)
# the following line hand-sets the weights
# nnet can be trained automatically by providing real data above and removing the following line
nn$wts <- W
x <- (-20:40) * 0.05
y <- x
xy <- expand.grid(x, y)

h1 <- data.matrix(xy)%*%matrix(c(n1WeightX1,n1WeightX2),nrow=2, ncol=1) + n1Offset
h1 <- 1.0 / (1.0 + exp(-h1))
h1 <- matrix(h1,nrow=61,ncol=61,byrow=TRUE)
contour(x,y,h1,col='darkorange')

h2 <- data.matrix(xy)%*%matrix(c(n2WeightX1,n2WeightX2),nrow=2, ncol=1) + n2Offset
h2 <- 1.0 / (1.0 + exp(-h2))
h2 <- matrix(h2,nrow=61,ncol=61,byrow=TRUE)
contour(x,y,h2,add=TRUE,col='darkolivegreen4')

z <- predict(nn, xy)
z <- matrix(z,nrow=61,ncol=61,byrow=TRUE)
contour(x,y,z,add=TRUE,col='black')

points(x1,x2,pch=23,bg=c("red","blue")[yData+1],add=TRUE)




library(nnet)

# XOR data (blue and red points)
x1=c(1, 1, 0.9, 1, 1, 0.9, 0, 0, 0.1, 0, 0, 0.1 )
x2=c(1, 0.9, 1, 0, 0.1, 0, 1, 0.9, 1, 0, 0.1, 0 )
xData = cbind(x1,x2)
yData=c(0,0,0,1,1,1,1,1,1,0,0,0)

nn <- nnet(xData,yData, size=50,maxit=250)

x <- (-5:25) * 0.05
y <- x
xy <- expand.grid(x, y)
z <- predict(nn, xy)
z <- matrix(z,nrow=31,ncol=31,byrow=TRUE)
contour(x,y,z)
points(x1,x2,pch=23,bg=c("red","blue")[yData+1],add=TRUE)
