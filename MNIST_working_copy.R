#MNIST
source("load_data.R")
library(ggplot2)

# Dimension reduction -----------------------------------------------------

# No Scaling, this has issues as some dimensions don't have any variation
train.pca = prcomp(train$x)
# Following have too many dimensions to get any useful information out of
#summary(train.pca)
#plot(train.pca)
#train.pca

eigs = train.pca$sdev^2
#calculate the cumulative probs
Cumulative=data.frame(
  `Cumulative Proportion`=cumsum(eigs)/sum(eigs),
  Dimensions=1:28^2
                     )
# 80
plot(x=seq(1,28^2,1),y=Cumulative$Cumulative.Proportion,type="l")
ggplot(Cumulative,aes(x=Dimensions,y=100*`Cumulative.Proportion`))+
  geom_line()+
  geom_hline(yintercept = 70,linetype="dashed")+
  geom_hline(yintercept = 80,linetype="dashed")+
  geom_hline(yintercept = 90,linetype="dashed")+
  geom_text(aes(x=26,y=80,label=paste("26")),col="red")+
  geom_text(aes(x=26,y=75,label=paste("↓")),col="red")+
  geom_text(aes(x=44,y=90,label=paste("44")),col="red")+
  geom_text(aes(x=44,y=85,label=paste("↓")),col="red")+
  geom_text(aes(x=87,y=100,label=paste("87")),col="red")+
  geom_text(aes(x=87,y=95,label=paste("↓")),col="red")+
  scale_y_continuous(breaks = seq(0,100,20))+
  labs(title = "PCA Dimension against proportion of varience explained",y="Cumulative Proportion (%)")+
  theme_bw()

  ## We want the PCA eigen vectors to account for at least 80%
# Lowest number of dimensionsf for this is 44.
numComponents=44 #80%
numComponents2=87  # 90%
numComponents3=26  # 70%

Xhat = t(t(train.pca$x[,1:numComponents]
           %*%
             t(train.pca$rotation[,1:numComponents])) 
           + train.pca$center)
Xhat2 = t(t(train.pca$x[,1:numComponents2]
           %*%
             t(train.pca$rotation[,1:numComponents2])) 
         + train.pca$center)
Xhat3 = t(t(train.pca$x[,1:numComponents3]
            %*%
              t(train.pca$rotation[,1:numComponents3])) 
          + train.pca$center)

# compare first few numbers against predicted version
train2=list()
Xhat.2=list()
Xhat2.2=list()
Xhat3.2=list()
for(i in 1:10){
  train2[[i]]=matrix(train$x[i,], nrow=28)[,28:1]
  dimnames(train2[[i]])=list(x2=1:28,y2=1:28)
  train2[[i]]=as.data.frame(as.table(train2[[i]]))
  train2[[i]]$number=train$y[i]
  train2[[i]]$id=i
  
  Xhat.2[[i]]=matrix(Xhat[i,], nrow=28)[,28:1]
  dimnames(Xhat.2[[i]])=list(x2=1:28,y2=1:28)
  Xhat.2[[i]]=as.data.frame(as.table(Xhat.2[[i]]))
  Xhat.2[[i]]$number=train$y[i]
  Xhat.2[[i]]$id=i
  
  Xhat2.2[[i]]=matrix(Xhat2[i,], nrow=28)[,28:1]
  dimnames(Xhat2.2[[i]])=list(x2=1:28,y2=1:28)
  Xhat2.2[[i]]=as.data.frame(as.table(Xhat2.2[[i]]))
  Xhat2.2[[i]]$number=train$y[i]
  Xhat2.2[[i]]$id=i
  
  Xhat3.2[[i]]=matrix(Xhat3[i,], nrow=28)[,28:1]
  dimnames(Xhat3.2[[i]])=list(x2=1:28,y2=1:28)
  Xhat3.2[[i]]=as.data.frame(as.table(Xhat3.2[[i]]))
  Xhat3.2[[i]]$number=train$y[i]
  Xhat3.2[[i]]$id=i
}
train3=do.call(rbind,train2)
Xhat.3=do.call(rbind,Xhat.2)
Xhat2.3=do.call(rbind,Xhat2.2)
Xhat3.3=do.call(rbind,Xhat3.2)
train3$x2=as.integer(train3$x2)
train3$y2=as.integer(train3$y2)
Xhat.3$x2=as.integer(Xhat.3$x2)
Xhat.3$y2=as.integer(Xhat.3$y2)
Xhat2.3$x2=as.integer(Xhat2.3$x2)
Xhat2.3$y2=as.integer(Xhat2.3$y2)
Xhat3.3$x2=as.integer(Xhat3.3$x2)
Xhat3.3$y2=as.integer(Xhat3.3$y2)
Xhat.3$pred="80%"
Xhat2.3$pred="90%"
Xhat3.3$pred="70%"
train3$pred="Real"
train4=rbind(train3,Xhat.3,Xhat2.3,Xhat3.3)

ggplot(train4,aes(x=x2,y=y2,fill=Freq))+geom_tile()+facet_grid(id~pred)+scale_fill_gradient(low="white",high="black")+labs(x="",y="",fill="Colour",title="Dimensionaly reduced MNIST dataset",subtitle = "Reconstructed for the first 10 values by variance explained")


# compare the average against the average predicted version (0-9)


# 3) K- nearest neighbour (KNN) -------------------------------------------
library(class)

test.pca = prcomp(test$x) # Reduce test data into 44 dimensions

knn_train=as.data.frame.array(train.pca$x[,1:numComponents2]) # create train dataset with 44 dimension reduced variables from PCA 
#knn_test=as.data.frame.array(test.pca$x[,1:numComponents]) # creat test dataset with 44 dimension reduced variables
knn_test=predict(train.pca,newdata = test$x)[,1:numComponents2]

# Single test first
results=knn(train = knn_train,test = knn_test,cl=train$y,k=5)
100*sum(as.numeric(as.character(results))==test$y,na.rm=T)/length(results==test$y)
table(`Actual Class` = test$y, `Predicted Class` = results)

# Plot to see if histogram of results and real data is similar
hist.knn=data.frame(
  number=c(as.numeric(as.character(results)),as.numeric(test$y)),
  type=c(rep("results",length(results)),rep("test data",length(test$y)))
)
ggplot(hist.knn,aes(number,fill=type))+geom_histogram(position="dodge")+theme_bw()#+facet_grid(~type)

# Need a way to find K!!!


# KNN with Caret ----------------------------------------------------------
 library(caret)
 train_control<- trainControl(method="cv", number=10, savePredictions = TRUE)
 
 knn_train$y=as.factor(train$y)
 
 model<- train(y~., data=knn_train, trControl=train_control, method="knn",tuneGrid   = expand.grid(k = seq(1,21,by=2)))
 
 plot(model)
 
 resultscv=predict(model,newdata=knn_test,type="raw")
 100*sum(as.numeric(as.character(resultscv))==test$y,na.rm=T)/length(resultscv==test$y)
 table(`Actual Class` = test$y, `Predicted Class` = resultscv)
 
 
 # Plot to see if histogram of results and real data is similar
 hist.knn=data.frame(
   number=c(as.numeric(as.character(resultscv)),as.numeric(test$y)),
   type=c(rep("results",length(resultscv)),rep("test data",length(test$y)))
 )
 ggplot(hist.knn,aes(number,fill=type))+geom_histogram(position="dodge")+theme_bw()#+facet_grid(~type)
 
 

# Next part.... -----------------------------------------------------------

library(e1071) 
yTable=table(train$y)
percentage=round(100*yTable/sum(yTable))
labels=paste0(row.names(yTable),"(",percentage,"%)")
pie(yTable,labels=labels,main="Total Number of Digits (training set)")

train2=train
train2$y=as.factor(train2$y)
train2[['n']]=NULL
train3=as.data.frame(train2$x)
model.naiveBayes <- naiveBayes(train2$y ~ ., data=train3) 
summary(model.naiveBayes) 

prediction.naiveBayes=predict(model.naiveBayes,newdata=test$x)
table(`Actual Class`=test$y,`Predicted Class`=prediction.naiveBayes)


Accuracy=100*sum(as.numeric(as.character(prediction.naiveBayes))==test$y,na.rm=T)/length(prediction.naiveBayes==test$y)


##   Now with PCA

train.pca = prcomp(train$x)

train2=train
train2$y=as.factor(train2$y)
train2[['n']]=NULL
train3=as.data.frame(train.pca$x[,1:numComponents])
#test.pca = prcomp(test$x)
test.pca=predict(train.pca,newdata = test$x)[,1:numComponents]

model.naiveBayes <- naiveBayes(train2$y ~ ., data=train3) 
summary(model.naiveBayes) 

prediction.naiveBayes=predict(model.naiveBayes,newdata=test.pca)
table(`Actual Class`=test$y,`Predicted Class`=prediction.naiveBayes)


Accuracy=100*sum(as.numeric(as.character(prediction.naiveBayes))==test$y,na.rm=T)/length(prediction.naiveBayes==test$y)


## Need method to visualise the predictions?? If this is possible.