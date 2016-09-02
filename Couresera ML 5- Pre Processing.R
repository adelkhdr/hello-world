library(caret)
library(ggplot2)
library(kernlab)
data(spam)
summary(spam)     
inTrain <- createDataPartition(spam$type,p=.75,list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

hist(training$capitalAve,main = "",xlab = "ave. capital")

# since the data shows a great standard deviation we need to standardize it
trainCap <- training$capitalAve
trainCapS <- (trainCap-mean(trainCap))/sd(trainCap)
mean(trainCapS)
sd(trainCapS)
hist(trainCapS,main="")

# Using a class of "caret" library to preprocess the data
preObj=preProcess(training[,-58],method = c("center","scale"))
trainCapAvs=predict(preObj,training[,-58])$capitalAve
mean(trainCapAvs)
sd(trainCapAvs)
# applying the current object on a different dataset
testCapAvs=predict(preObj,testing[,-58])$capitalAve
# we can also apply the preporcess as an argument in the fit method
modelfit<- train(type~., data = training,preProcess=c("center","scale"),method="glm")

# Using other kind ot transformation
preObj2=preProcess(training[,-58],method = c("BoxCox"))
trainCapS=predict(preObj2,training[,-58])$capitalAve
par(mfrow=c(1,2));hist(trainCapS);qqnorm(trainCapS)

# Working with Null Data
set.seed(13433)
training$capval=training$capitalAve
slectNA= rbinom(dim(training)[1],size=1, prob =.5)==1
training$capval[slectNA]=NA
# Impute and standardization
preObj3=preProcess(training[,-58],method = c("knnImpute"))
capAve=predict(preObj3,training[,-58])$capval









