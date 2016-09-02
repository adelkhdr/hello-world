# this is a program to detect the spam based on the average number of capital letters
#install.packages('caret', dependencies = TRUE)
#install.packages("kernlab")
library(kernlab)
data("spam")
smallSpam <- spam[sample(dim(spam)[1],size=10),]
spamlabel <- (smallSpam$type=="spam")*1+1
plot(smallSpam$capitalAve,col=spamlabel)


# Defining the algorithm of detecting spam
rule <- function(x) {
  prediction <- rep(NA,length(x))
  prediction[x>2.8] <- "spam"
  prediction[x<=2.8] <- "nonspam"
  return(prediction)
}
table(rule(smallSpam$capitalAve),smallSpam$type)
