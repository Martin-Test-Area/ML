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
  geom_hline(yintercept = 80)+
  geom_text(aes(x=44,y=90,label=paste("44")),col="red")+
  #geom_text(aes(x=44,y=85,label=paste("|")),col="red")+
  #geom_text(aes(x=44,y=82,label=paste("˅")),col="red")+
  geom_text(aes(x=44,y=85,label=paste("↓")),col="red")+
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

ggplot(train4,aes(x=x2,y=y2,fill=Freq))+geom_tile()+facet_grid(id~pred)+scale_fill_gradient(low="white",high="black")


# compare the average against the average predicted version (0-9)


# W/ scaled ---------------------------------------------------------------
# 
# #scaling isn't need as black scaled between 0 and 255
# # With Scaling
# norm.vec=function(x){
#   x/sqrt(sum(x^2))
# }
# 
# train.scaled=norm.vec(train$x)
# #scaling.vector=
# 
# train.scaled.pca = prcomp(train.scaled,scale. = F)
# 
# eigs = train.scaled.pca$sdev^2
# #calculate the cumulative probs
# Cumulative=data.frame(
#   `Cumulative Proportion`=cumsum(eigs)/sum(eigs),
#   Dimensions=1:28^2
# )
# # 80
# #plot(x=seq(1,28^2,1),y=Cumulative,type="l")
# ggplot(Cumulative,aes(x=Dimensions,y=100*`Cumulative.Proportion`))+
#   geom_line()+
#   geom_hline(yintercept = 80)+
#   geom_text(aes(x=44,y=90,label=paste("44")),col="red")+
#   #geom_text(aes(x=44,y=85,label=paste("|")),col="red")+
#   #geom_text(aes(x=44,y=82,label=paste("˅")),col="red")+
#   geom_text(aes(x=44,y=85,label=paste("↓")),col="red")+
#   scale_y_continuous(breaks = seq(0,100,20))+
#   labs(title = "PCA Dimension against proportion of varience explained",y="Cumulative Proportio (%)")+
#   theme_bw()
# 
# 
# ## We want the PCA eigen vectors to account for at least 80%
# # Lowest number of dimensionsf for this is 44.
# numComponents=44 #80%
# numComponents2=87  # 90%
# numComponents3=154  # 95%
# 
# Xhat = t(t(train.scaled.pca$x[,1:numComponents]
#            %*%
#              t(train.scaled.pca$rotation[,1:numComponents])) *
#          387000 + train.scaled.pca$center)
# Xhat2 = t(t(train.scaled.pca$x[,1:numComponents2]
#             %*%
#               t(train.scaled.pca$rotation[,1:numComponents2])) *
#             387000  + train.scaled.pca$center)
# Xhat3 = t(t(train.scaled.pca$x[,1:numComponents3]
#             %*%
#               t(train.scaled.pca$rotation[,1:numComponents3])) *
#             387000 + train.scaled.pca$center)
# 
# # compare first few numbers against predicted version
# train2=list()
# Xhat.2=list()
# Xhat2.2=list()
# Xhat3.2=list()
# for(i in 1:3){
#   train2[[i]]=matrix(train$x[i,], nrow=28)[,28:1]
#   dimnames(train2[[i]])=list(x2=1:28,y2=1:28)
#   train2[[i]]=as.data.frame(as.table(train2[[i]]))
#   train2[[i]]$number=train$y[i]
#   train2[[i]]$id=i
#   
#   Xhat.2[[i]]=matrix(Xhat[i,], nrow=28)[,28:1]
#   dimnames(Xhat.2[[i]])=list(x2=1:28,y2=1:28)
#   Xhat.2[[i]]=as.data.frame(as.table(Xhat.2[[i]]))
#   Xhat.2[[i]]$number=train$y[i]
#   Xhat.2[[i]]$id=i
#   
#   Xhat2.2[[i]]=matrix(Xhat2[i,], nrow=28)[,28:1]
#   dimnames(Xhat2.2[[i]])=list(x2=1:28,y2=1:28)
#   Xhat2.2[[i]]=as.data.frame(as.table(Xhat2.2[[i]]))
#   Xhat2.2[[i]]$number=train$y[i]
#   Xhat2.2[[i]]$id=i
#   
#   Xhat3.2[[i]]=matrix(Xhat3[i,], nrow=28)[,28:1]
#   dimnames(Xhat3.2[[i]])=list(x2=1:28,y2=1:28)
#   Xhat3.2[[i]]=as.data.frame(as.table(Xhat3.2[[i]]))
#   Xhat3.2[[i]]$number=train$y[i]
#   Xhat3.2[[i]]$id=i
# }
# train3=do.call(rbind,train2)
# Xhat.3=do.call(rbind,Xhat.2)
# Xhat2.3=do.call(rbind,Xhat2.2)
# Xhat3.3=do.call(rbind,Xhat3.2)
# train3$x2=as.integer(train3$x2)
# train3$y2=as.integer(train3$y2)
# Xhat.3$x2=as.integer(Xhat.3$x2)
# Xhat.3$y2=as.integer(Xhat.3$y2)
# Xhat2.3$x2=as.integer(Xhat2.3$x2)
# Xhat2.3$y2=as.integer(Xhat2.3$y2)
# Xhat3.3$x2=as.integer(Xhat3.3$x2)
# Xhat3.3$y2=as.integer(Xhat3.3$y2)
# Xhat.3$pred="80%"
# Xhat2.3$pred="90%"
# Xhat3.3$pred="95%"
# train3$pred="Real"
# train4=rbind(train3,Xhat.3,Xhat2.3,Xhat3.3)
# 
# ggplot(train4,aes(x=x2,y=y2,fill=Freq))+geom_tile()+facet_grid(number~pred)+scale_fill_gradient(low="white",high="black")





# 3) K- nearest neighbour (KNN) -------------------------------------------
library(class)

test.pca = prcomp(test$x) # Reduce test data into 44 dimensions

knn_train=as.data.frame.array(train.pca$x[,1:numComponents]) # create train dataset with 44 dimension reduced variables from PCA 
#knn_test=as.data.frame.array(test.pca$x[,1:numComponents]) # creat test dataset with 44 dimension reduced variables
knn_test=predict(train.pca,newdata = test$x)[,1:numComponents]

# Single test first
results=knn(train = knn_train,test = knn_test,cl=train$y,k=5)
100*sum(as.numeric(as.character(results))==test$y,na.rm=T)/length(results==test$y)
table(`Actual Class` = test$y, `Predicted Class` = results)

# Plot to see if histogram of results and real data is similar
hist.knn=data.frame(
  number=c(as.numeric(as.character(results)),as.numeric(test$y)),
  type=c(rep("results",length(results)),rep("test data",length(test$y)))
)
ggplot(hist.knn,aes(number,fill=type))+geom_histogram(position="dodge")#+facet_grid(~type)

# Need a way to find K!!!


# KNN with Caret ----------------------------------------------------------
 library(caret)
 train_control<- trainControl(method="cv", number=10, savePredictions = TRUE)
 
 knn_train$y=as.factor(train$y)
 
 model<- train(y~., data=knn_train, trControl=train_control, method="knn",tuneGrid   = expand.grid(k = 1:10))
 
 plot(model)
 
 resultscv=predict(model,newdata=knn_test,type="raw")
 100*sum(as.numeric(as.character(resultscv))==test$y,na.rm=T)/length(resultscv==test$y)
 table(`Actual Class` = test$y, `Predicted Class` = resultscv)
 