library(caret)
library(kernlab)
data(spam)

# A function to separate index of the data to train and test sets
inTrain <- createDataPartition(y=spam$type,p=0.75,list = FALSE)
training<- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

# Cross validation approach using K-fold method
set.seed(32323)
folds <- createFolds(y=spam$type,k=10,list=TRUE,returnTrain = FALSE)
sapply(folds, length)
folds[[2]][1:30]

# Resampling
set.seed(32323)
folds <- createResample(y=spam$type, times = 10,list = TRUE)
sapply(folds, length)

# Time slice
set.seed(32323)
folds <- createTimeSlices(y=spam$type,initialWindow = 20,horizon = 10)
names(folds)
folds$train[[2]][]
