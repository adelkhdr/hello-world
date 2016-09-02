library(caret)
library(kernlab)
data(spam)

# A function to separate index of the data to train and test sets
inTrain <- createDataPartition(y=spam$type,p=0.75,list = FALSE)
training<- spam[inTrain,]
testing <- spam[-inTrain,]

# Making the model
set.seed(1235)
modelfit <- train(type~., data = training, method="glm")
modelfit