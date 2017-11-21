# Classification with rpart package - Covertype Data 
#############################################################
#install and load packages: rpart, rpart.plot, caret
packages <- c(
  "rpart",
  "rpart.plot",
  "caret"
)

for (package in packages)
{
  if (!require(package, character.only=TRUE))
  {
    install.packages(package, dep = TRUE, character.only=TRUE)
    library(package, character.only=TRUE)
  }
}

#############################################################
### Load data
# find details about Covertype data: http://archive.ics.uci.edu/ml/datasets/Covertype
covtype <- read.csv("F:/R/作业/decision tree/covtype.csv", header = FALSE)

class(covtype)
#############################################################
### Explore data
dim(covtype)
colnames(covtype)
summary(covtype)
head(covtype)
tail(covtype)

#histogram of the label column

hist(covtype$V55)

# change the lable column to factor  
covtypeDF <- covtype

covtypeDF$V55 <- factor(covtype$V55)
summary(covtypeDF)
#use bar chart to show the frequency of categorical variable 
barchart(covtypeDF$V55)

#############################################################
### splitting data into train and test 
#Simple Splitting Based on the Outcome using creatDataPartition
#By default, createDataPartition does a stratified random split of the data

set.seed(30)
trainIndex <- createDataPartition(y = covtypeDF$V55, # the outcome variable
                                  times = 1,
                                  p = .7, # The percentage of data in the training set
                                  list = FALSE # 
                                  )

# The output is a set of integers for the rows of the data that belong to the training set
class(trainIndex)
dim(trainIndex)
head(trainIndex)

#To split the data
covtypeTrain <- covtypeDF[ trainIndex,]
covtypeTest  <- covtypeDF[-trainIndex,]

nrow(covtypeTrain)
nrow(covtypeTest)

barchart(covtypeTrain$V55)
barchart(covtypeTest$V55)

summary(covtypeTrain)


#############################################################
### build a decision tree 
covtypeTree <- rpart(formula = V55 ~ .,
                     data = covtypeTrain,
                     #parms = ,
                     #control = rpart.control(cp = 0), 
                     method = "class"
                    )

covtypeTree
class(covtypeTree)
?rpart.object
summary(covtypeTree)
printcp(covtypeTree)
plotcp(covtypeTree)

plot(covtypeTree); text(covtypeTree)
rpart.plot(covtypeTree)


#############################################################
### prediction
covtypePredict <- predict(covtypeTree,
                          newdata=covtypeTest,
                          type='class'
                          )
class(covtypePredict)
head(covtypePredict)
tail(covtypePredict)

#############################################################
### evalaution 
# make a contingency table 
covtypeTestTable <- table(covtypePredict, covtypeTest$V55)
covtypeTestTable

# use confusionmatrix() from caret package
?confusionMatrix
covtypeConfusionMatrix <- confusionMatrix(covtypePredict, covtypeTest$V55)
covtypeConfusionMatrix

# get accuracy
covtypeTreeAccuracy <- covtypeConfusionMatrix$overall[1]


#############################################################
### build a tree with cp=0
covtypeTreeCP0 <- rpart(formula = V55 ~ .,
                     data = covtypeTrain,
                     control = rpart.control(cp = 0), 
                     method = "class"
                  )# this may take a long time
covtypeTreeCP0 
rpart.plot(covtypeTreeCP0) # this may take a long time

#predict
covtypePredictCP0 <- predict(covtypeTreeCP0,
                          newdata=covtypeTest,
                          type='class'
)

covtypeConfusionMatrixCP0 <- confusionMatrix(covtypePredictCP0, covtypeTest$V55)
covtypeConfusionMatrixCP0
#############################################################
# build the tree with different parameters and controls in the rpart function
?rpart.control 

# compare with the previous tree
# predict and compare with the previous results 
