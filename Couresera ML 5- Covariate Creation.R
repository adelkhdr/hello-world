library(ISLR)
library(caret)
library(ggplot2)
data(Wage)
#summerizing the data
summary(Wage)     
# Making the training and testing sets
inTrain <- createDataPartition(Wage$wage,p=.7,list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

table(training$jobclass)

# Adding dummy variables to change qualitative vars to quantitative
dummies <- dummyVars (wage~jobclass,data = training)
head(predict(dummies,newdata=training))
head(training)

# NZV: near zero variation to detect the predictors that do not have much vairability to detect
# meaningful variables
nsv <- nearZeroVar(training,saveMetrics = TRUE)
nsv

# Drwaing Splines to find the trends
library(splines)
bsBasis <- bs(training$age,df=3)  # turns 3 polynomial degree approx of age: age+age^2+age^3
bsBasis
lm1=lm(wage~bsBasis,data = training)
plot(training$age,training$wage, pch=19, cex=0.4)
points(training$age,predict(lm1, newdata=training),col='blue',pch=19,cex=.7)
