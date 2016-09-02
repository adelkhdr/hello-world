library(caret)
library(ggplot2)
library(kernlab)
data(spam)
summary(spam)     
inTrain <- createDataPartition(spam$type,p=.75,list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
#taking the correlations of variables
M<- abs(cor(spam[,-58]))
diag(M)<-0
which(M>0.8, arr.ind = T)
plot(spam[,34],spam[,32])

# Performing PCA
SmalSpam=spam[,c(32,34)]
prComp=prcomp(SmalSpam)
plot(prComp$x[,1],prComp$x[,2])
prComp$rotation

# PCA on spam data
typeColor=(spam$type=="spam")*1+1
pcaSpam=prcomp(log10(spam[,-58]+1))
plot(pcaSpam$x[,1],pcaSpam$x[,2],col=typeColor)

# PCA with caret
preProc=preProcess(log10(spam[,-58]+1),method = "pca",pcaComp = 2)
SpamPCA=predict(preProc,log10(spam[,-58]+1))
plot(SpamPCA$PC1,SpamPCA$PC2,col=typeColor)
