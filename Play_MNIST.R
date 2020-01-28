source("load_data.R")

hist(train$y)

summary(train)

summary(train$x[train$y=="0"])
str(train)
library(dplyr)
library(tidyr)
train2=list()

# ggplot ------------------------------------------------------------------


for(i in 1:6){
train2[[i]]=matrix(train$x[i,], nrow=28)[,28:1]
dimnames(train2[[i]])=list(x2=1:28,y2=1:28)
train2[[i]]=as.data.frame(as.table(train2[[i]]))
train2[[i]]$number=train$y[i]
train2[[i]]$id=i
}


library(ggplot2)
ggplot(train2[[1]],aes(x=x2,y=y2,fill=Freq))+geom_tile()

train3=do.call(rbind,train2)
train3$x2=as.integer(train3$x2)
train3$y2=as.integer(train3$y2)

ggplot(train3,aes(x=x2,y=y2,fill=Freq))+geom_tile()+facet_grid(~number)



# format data -------------------------------------------------------------


# dimension reduction -----------------------------------------------------
train.pca = prcomp(train$x) # Wil need a work around if we're going to scale.=T
summary(train.pca)
plot(train.pca)
train.pca
#train.pca$x
#train.pca$rotation
#plot(train$x)
eigs = train.pca$sdev^2
#calculate the cumulative probs
Cumulative=cumsum(eigs)/sum(eigs)
plot(x=seq(1,28^2,1),y=Cumulative,type="l")
## We want the PCA eigen vectors to account for at least 80%
# Lowest number of dimensionsf for this is 44.
numComponents=44
Xhat = t(t(train.pca$x[,1:numComponents]
           %*%
             t(train.pca$rotation[,1:numComponents])) *
           train.pca$scale + train.pca$center)
show_digit(Xhat[1:28^2,])

testPCA = predict(train.pca, test)


#KNN test
library(class)
train.1=list()
train.2=list()
train.1$x=train$x[1:900,]
train.1$y=train$y[1:900]
train.2$x=train$x[901:1000,]
results=knn(train = train.1$x,test = train.2$x,cl=train.1$y,k=5)
100*sum(results==train$y[901:1000],na.rm=T)/length(results==train$y[901:1000])


#KNN k fold on data
train2=train$x
train2=as.data.frame.array(train2)
train2$y=train$y
train3=train2[501:1000,]
train2=train2[1:500,]

library(caret)
train_control<- trainControl(method="cv", number=10, savePredictions = TRUE)
model<- train(y~., data=train2, trControl=train_control, method="knn")
model$pred

resultscv=predict(model,newdata=train3,type="raw")
100*sum(resultscv==train3$y,na.rm=T)/length(resultscv==train3$y)



# KNN full real (96.88%)
results=knn(train = train$x,test = test$x,cl=train$y,k=5)
100*sum(results==test$y,na.rm=T)/length(results==test$y)
