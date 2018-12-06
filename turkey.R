
getwd()


# Load the package required to read JSON files.
library("rjson")

# Give the input file name to the function.
traindata <- fromJSON(file = "./data/train.json")
testdata=fromJSON(file="./data/test.json")

# Print the result.
# print(result)

# Convert JSON file to a data frame.
# trainDf <- as.data.frame(traindata) #not work
trainDf = data.frame(matrix(unlist(traindata), nrow=1195, byrow=T))
str(unlist(traindata))
vars=unique(unlist(lapply(traindata, names))) #names of top layer lists
vars
ae=unlist(lapply(traindata$audio_embedding, c()))
str(traindata[[1]]$audio_embedding) #List of 10  X128
str(traindata[[36]]$audio_embedding) #List of 9

#convert nested list to dataframe
library(data.table)
trainDf = rbindlist(traindata, fill=TRUE)
library(dplyr)
trainDf = bind_rows(traindata)

#
temp = unique(unlist(lapply(traindata, names)))
mydf = setNames(object = data.frame(lapply(temp, function(nm)
  unlist(lapply(traindata, function(x) x[[nm]])))), nm = temp)


head(trainDf) 
str(trainDf)
tail(colnames(trainDf))
tail(trainDf[, 1266:1268])

class(traindata)  #list 
class(trainDf)
str(trainDf)
str(traindata)
traindata[[1]][[1]]
str(traindata[[1]]) #List of 5
str(traindata[[1]][[1]]) #List of 10
str(traindata[[1]][[1]][[1]])  #num [1:128] 172 34 216 110 208 46 95 66 161 125 ...

str(traindata[[1000]][[2]])
traindata[[1000]][[2]]


traindata[[128]][[1]]
head(trainDf[,1:5])
tail(colnames(trainDf))
col=paste0("ae",seq(1:16571))
col=c(col, "isturkey", "id","end", "start")
colnames(trainDf)=col

head(colnames(trainDf))
tail(trainDf[,16570:16575])

### list to dataframe
# install.packages("qdapTools")
# library(qdapTools)
# trainDf=list2df(traindata)
###

#
#
# Load the party package. It will automatically load other
# required packages.
install.packages("party")
install.packages("randomForest")
library(party)
library(randomForest)

set.seed(1001)
tvar = setdiff(colnames(trainDf[,1:10]), list('id','isturkey'))
fmodel= randomForest(x=trainDf[, tvar], y=as.factor(trainDf$isturkey), importance = T)  #ntree = 100, nodesize = 7, 

unique(trainDf$isturkey)
tail(trainDf$isturkey)
summary(trainDf$isturkey)


accuracyMeasures(predict(fmodel, newdata = trainDf[, tvar], type="prob")[, 'isturkey'], trainDf$isturkey=="1", name="random forest, train")

varImp=importance(fmodel)
varImp[1:10, ]
varImpPlot(fmodel, type=1)

#fit with fewer variables
selVars = names(sort(varImp[,1], decreasing = T))[1:25]
fsel = randomForest(x=trainDf[,selVars], y=trainDf$isturkey, ntree=100, nodesize = 7, importance = T)

# Create the forest.
output.forest <- randomForest(isturkey ~ ., data = trainDf)
output.forest <- randomForest(isturkey ~ end+start, data = trainDf)
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




#################
#####

library(caret)
# inTrain <‐ createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
# training <‐ Wage[inTrain,]; testing <‐ Wage[‐inTrain,]
hist(training$is_turkey ,main="",xlab="is turkey")
hist(training$end_time_seconds_youtube_clip ,main="",xlab="end time")
hist(training$start_time_seconds_youtube_clip ,main="",xlab="start time")
table(training$is_turkey)
table(training$start_time_seconds_youtube_clip)
dummies = dummyVars(is_turkey ~ start_time_seconds_youtube_clip,data=training)
head(predict(dummies,newdata=training))
# Removing zero covariates
nsv = nearZeroVar(training[,2:5],saveMetrics=TRUE)
nsv

# convert matrix to list before build models
str(training$audio_embedding)

audiolist= traindata$audio_embedding[[36]]
head(audiolist, n=10)

audioList36= as.vector(training$audio_embedding[[36]])
audioList36=as.list(audioList36)

audioList1= as.vector(training$audio_embedding[[1]])
audioList1=as.list(audioList1)
hist(audioList1)
summary(audioList1)
head(audioList1)
adf=as.data.frame(audioList1)
adft=t(adf)
head(adft[,1:5])
adft[1,1:6]
colnames(adft)

str(traindata[[3]])
traindata[[3]]$is_turkey
traindata[[3]][[2]]
traindata[[3]][2]
traindata[[36]][1]

traindata[[3]]$audio_embedding[[2]]
str(traindata[[3]]$audio_embedding[[2]])
traindata[[3]]$audio_embedding
str(traindata[[3]]$audio_embedding)
unlist(traindata[[3]]$audio_embedding)
x=as.data.frame(unlist(traindata[[3]]$audio_embedding))
x=t(x)
x=as.data.frame(x)
##
library(plyr)
aud_df=data.frame(matrix(ncol = 1280, nrow = 0))
# aud_dft=data.frame()
col=paste("ae", as.character(seq(1280)), sep="")  
colnames(aud_df)=col

#Below is a test#
x=unlist(traindata[[36]]$audio_embedding)
length(x)=1280
tail(x)
y=unlist(traindata[[35]]$audio_embedding)
df=cbind(x,y)
df <- data.frame(cbind(df, y))
head(df)
rm(df)
rm(x,y,i)
# aud_vec=t(as.data.frame(aud_vec))
# aud_dft=as.data.frame(t(as.data.frame(aud_vec)))
if (length(aud_vec)==1280){ 
  colnames(aud_dft)=col 
  aud_df=rbind.fill(aud_df, aud_dft)
}
unique(colnames(aud_df))
head(aud_df[,10:15])
rm(aud_dft)
rm(trainDf, fmodel, varImp,x)
#test ends

#loop
aud_df=c()
for (i in seq_along(traindata))   {
  aud_vec=unlist(traindata[[i]]$audio_embedding)
  length(aud_vec)=1280 #if not 1280, will be coerced to 1280, empty ones convert to NA
  # aud_vec=t(as.data.frame(aud_vec))
  # aud_dft=as.data.frame(t(as.data.frame(aud_vec)))
  # colnames(aud_dft)=col  
  aud_df=data.frame(cbind(aud_df, aud_vec))
  
}
unique(colnames(aud_df))
tail(colnames(aud_df))

audDf=t(aud_df)
audDf = as.data.frame(audDf)
colnames(audDf)=col 
tail(audDf[31:36,1270:1280])  # show NA in 36th row
audDf[is.na(audDf)] <- 0      #https://stackoverflow.com/questions/10139284/set-na-to-0-in-r
tail(audDf[31:36,1270:1280]) 

#append the other 4 columns to form whole data frame.
str(traindata[[1]])  #check structure of one sample in the list

tl <- lapply(traindata, function(x) {x[c(2:5)]})  #https://stackoverflow.com/questions/9624169/how-to-subset-from-a-list-in-r
head(tl)
library(data.table)
td=rbindlist(tl, fill=TRUE)  #convert nested list to dataframe. as.data.frame does not work. #https://stackoverflow.com/questions/26177565/converting-nested-list-to-dataframe
head(td)
trainDf=cbind(td,audDf)
head(trainDf[,1:6])

# Removing zero covariates
nsv = nearZeroVar(audDf[,],saveMetrics=TRUE)
nsv

########
# Feature Selection #
set.seed(1001)
#https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
# calculate correlation matrix
library(caret)
tdCorMatrix <- cor(trainDf[,3:1284])
# summarize the correlation matrix
print(tdCorMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(tdCorMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)

##Rank Features By Importance
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
trainDf2=trainDf
trainDf2$is_turkey = as.factor(trainDf2$is_turkey)
# install.packages("e1071")
library(e1071)
colnames(trainDf2[,1:5])
trainDf3=trainDf2[,-2]  #remove id column, which is not useful
colnames(trainDf3[,1:5])
model <- train(is_turkey ~., data=trainDf3[,1:20], method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


#Feature Selection
# Automatic feature selection methods can be used to build many models with different subsets of a dataset and identify those attributes that are and are not required to build an accurate model.
# A popular automatic method for feature selection provided by the caret R package is called Recursive Feature Elimination or RFE.
# The example below provides an example of the RFE method on the Pima Indians Diabetes dataset. A Random Forest algorithm is used on each iteration to evaluate the model. The algorithm is configured to explore all possible subsets of the attributes. All 8 attributes are selected in this example, although in the plot showing the accuracy of the different attribute subset sizes, we can see that just 4 attributes gives almost comparable results.
# Automatically select features using Caret R PackageR

# ensure the results are repeatable

trainDf4=trainDf3[,-1]
trainDf4 <- trainDf4[,-nearZeroVar(trainDf4)]
colnames(trainDf4[,1:6])
tdCorMatrix2 <- cor(trainDf4)
# summarize the correlation matrix
print(tdCorMatrix2)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated2 <- findCorrelation(tdCorMatrix2, cutoff=0.75)
length(highlyCorrelated2)
head(highlyCorrelated2)

library(data.table)
trainDf5=setDF(trainDf4)  #change class to data.frame. because data.table can't do column subset by indices.
class(trainDf4)
trainDf5=trainDf4[, -highlyCorrelated2]
colnames(trainDf5[,1:6])

set.seed(7)
# load the library
library(caret)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
?rfe
results <- rfe(trainDf5[,1:8], trainDf3[,1], sizes=c(2:8), rfeControl=control)


# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
#####


#predict with random forest
library(caret)
modFit = train(is_turkey ~ .,data=trainDf,method="rf",prox=TRUE)

modFit = train(is_turkey ~ ae3+ae4+ae14+ae13+ae15, data=trainDf3,method="rf",prox=TRUE)

modFit


#test data
testDf = data.frame(matrix(unlist(testdata), nrow=1196, byrow=T))
vars_test=unique(unlist(lapply(testdata, names))) #names of top layer lists
vars_test
ae_test=unlist(lapply(traindata$audio_embedding, c()))
str(testdata[[1]]$audio_embedding) #List of 10  X128
str(testdata[[36]]$audio_embedding) #List of 9

#convert nested list to dataframe
##
library(plyr)
# aud_df_test=data.frame(matrix(ncol = 1280, nrow = 0))

col=paste("ae", as.character(seq(1280)), sep="")  
colnames(aud_df_test)=col

#loop
aud_df_test=c()
for (i in seq_along(testdata))   {
  aud_vec_test=unlist(testdata[[i]]$audio_embedding)
  length(aud_vec_test)=1280 #if not 1280, will be coerced to 1280, empty ones convert to NA
  aud_df_test=data.frame(cbind(aud_df_test, aud_vec_test))
  
}
unique(colnames(aud_df_test))
tail(colnames(aud_df_test))
head(colnames(aud_df_test))

audDf_test=t(aud_df_test)
audDf_test = as.data.frame(audDf_test)
colnames(audDf_test)=col 
tail(audDf_test[31:36,1270:1280])  # show NA in 36th row
audDf_test[is.na(audDf_test)] <- 0      #https://stackoverflow.com/questions/10139284/set-na-to-0-in-r
tail(audDf_test[31:36,1270:1280]) 

#append the other 4 columns to form whole data frame.
str(testdata[[10]])  #check structure of one sample in the list

tl_test <- lapply(testdata, function(x) {x[c(2:4)]})  #test data lacks is_turkey, which will be predicted
head(tl_test)
library(data.table)
td_test=rbindlist(tl_test, fill=TRUE)  #convert nested list to dataframe. as.data.frame does not work. #https://stackoverflow.com/questions/26177565/converting-nested-list-to-dataframe
head(td_test)
testDf=cbind(td_test,audDf_test)
head(testDf[,1:6])


# Predicting new values
pred = predict(modFit,testDf) 
head(pred)
tail(pred)
write.csv(pred, "prediction.csv")

testDf$predRight = pred==testing$Species
table(pred,testing$Species)
qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,main="newdata Predictions")






