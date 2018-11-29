
getwd()
traindata=read.csv("./train.json")

# Load the package required to read JSON files.
library("rjson")

# Give the input file name to the function.
traindata <- fromJSON(file = "./data/train.json")
testdata=fromJSON(file="./data/test.json")

# Print the result.
print(result)

# Convert JSON file to a data frame.
trainDf <- as.data.frame(traindata)

class(traindata)  #list
class(trainDf)
str(trainDf)
str(traindata)
traindata[[1]][[1]]
str(traindata[[1]]) #List of 5
str(traindata[[1]][[1]]) #List of 10
str(traindata[[1]][[1]][[1]])  #num [1:128] 172 34 216 110 208 46 95 66 161 125 ...

str(traindata[[10]][[4]])

traindata[[128]][[1]]
head(trainDf[,1:5])
tail(colnames(trainDf))
col=paste0("ae",seq(1:16571))
col=c(col, "isturkey", "id","end", "start")
colnames(trainDf)=col

head(colnames(trainDf))
tail(trainDf[,16570:16575])

#
#
# Load the party package. It will automatically load other
# required packages.
install.packages("party")
install.packages("randomForest")
library(party)
library(randomForest)

# Create the forest.
output.forest <- randomForest(isturkey ~ ., data = trainDf)

# View the forest results.
print(output.forest) 

# Importance of each predictor.
print(importance(fit,type = 2)) 


#To make our formula for RF easier to manipulate

var.predict<-paste(names(iris)[-5],collapse="+")
rf.form <- as.formula(paste(names(iris)[5], var.predict, sep = " ~ "))

print(rf.form)
#This is our current itteration of the formula we're using in RF

iris.rf<-randomForest(rf.form,data=iris,importance=TRUE,ntree=100)

varImpPlot(iris.rf)
#Examine our Variable importance plot

to.remove<-c(which(data.frame(iris.rf$importance)$MeanDecreaseAccuracy==min(data.frame(iris.rf$importance)$MeanDecreaseAccuracy)))
#Remove the variable with the lowest decrease in Accuracy (Least relevant variable)

#Rinse, wash hands, repeat

var.predict<-paste(names(iris)[-c(5,to.remove)],collapse="+")
rf.form <- as.formula(paste(names(iris)[5], var.predict, sep = " ~ "))

iris.rf<-randomForest(rf.form,data=iris,importance=TRUE,ntree=100)

varImpPlot(iris.rf)
#Examine our Variable importance plot

to.remove<-c(to.remove, which(data.frame(iris.rf$importance)$MeanDecreaseAccuracy==min(data.frame(iris.rf$importance)$MeanDecreaseAccuracy)))










