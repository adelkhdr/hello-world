# install.packages("ISLR")
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
dim(training); dim(testing)

# Plotting the descriptive data
featurePlot(x=training[,c("age","education","jobclass")],y=training$age,plot = "pairs")
qplot(age,wage,data = training)
qq <- qplot(age,wage,colour=education,data = training)

# Adding regression smoothers
qq+geom_smooth(method ="lm",formula =y~x )


# A package for spliting the sets
library(Hmisc)
library(gridExtra)  #a library for grid.arragrid.arrange 
cutwage <- cut2(training$wage,g=3)
table(cutwage)
p1 <- qplot(cutwage,age,data = training,fill=cutwage,geom = c("boxplot"))
p2 <- qplot(cutwage,age,data = training,fill=cutwage,geom = c("boxplot","jitter"))
grid.arrange (p1,p2,ncol=2)

t1 <- table(cutwage,training$jobclass)
prop.table(t1,1)   # getting the proportion table -1 indicates the proportion of rows

# Plotting the densities  
p2 <- qplot(wage,colour=education,data = training,geom = "density")
p2
