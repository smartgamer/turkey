
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



#
#
# Load the party package. It will automatically load other
# required packages.
library(party)
library(randomForest)

# Create the forest.
output.forest <- randomForest(nativeSpeaker ~ age + shoeSize + score, 
                              data = readingSkills)

# View the forest results.
print(output.forest) 

# Importance of each predictor.
print(importance(fit,type = 2)) 


















































