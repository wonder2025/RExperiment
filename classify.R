# Listing 17.1 - Prepare the breast cancer data
loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
ds  <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"
url <- paste(loc, ds, sep="")

breast <- read.table(url, sep=",", header=FALSE, na.strings="?")
breast
dim(breast)
names(breast) <- c("ID", "clumpThickness", "sizeUniformity",
                   "shapeUniformity", "maginalAdhesion", 
                   "singleEpithelialCellSize", "bareNuclei", 
                   "blandChromatin", "normalNucleoli", "mitosis", "class")

breast
df <- breast[-1]
df
df$class <- factor(df$class, levels=c(2,4), 
                   labels=c("benign", "malignant"))

str(df)
set.seed(1234)
train <- sample(nrow(df), 0.7*nrow(df))
train 
df.train <- df[train,]
df.validate <- df[-train,]
table(df.train$class)
table(df.validate$class)

# Listing 17.6 - A support vector machine
library(e1071)
set.seed(1234)
fit.svm <- svm(class~., data=df.train)
fit.svm
svm.pred <- predict(fit.svm, na.omit(df.validate))
na.omit(df.validate)$class

svm.perf <- table(na.omit(df.validate)$class, 
                  svm.pred, dnn=c("Actual", "Predicted"))
svm.perf


# Listing 17.7 Tuning an RBF support vector machine (this can take a while)
set.seed(1234)
tuned <- tune.svm(class~., data=df.train,
                  gamma=10^(-6:1),
                  cost=10^(-10:10))
tuned
fit.svm1 <- svm(class~., data=df.train, gamma=0.1, cost=1)
svm.pred1 <- predict(fit.svm1, na.omit(df.validate))
svm.perf1 <- table(na.omit(df.validate)$class,
                  svm.pred1, dnn=c("Actual", "Predicted"))
svm.perf

fit.svm2 <- svm(class~., data=df.train, gamma=1e-06, cost=1e+07)
svm.pred2 <- predict(fit.svm2, na.omit(df.validate))
svm.perf2 <- table(na.omit(df.validate)$class,
                  svm.pred2, dnn=c("Actual", "Predicted"))
svm.perf2



# Listing 17.8 Function for assessing binary classification accuracy
performance <- function(table, n=2){
  if(!all(dim(table) == c(2,2)))
    stop("Must be a 2 x 2 table")
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  sensitivity = tp/(tp+fn)
  specificity = tn/(tn+fp)
  ppp = tp/(tp+fp)
  npp = tn/(tn+fn)
  hitrate = (tp+tn)/(tp+tn+fp+fn)
  result <- paste("Sensitivity = ", round(sensitivity, n) ,
                  "\nSpecificity = ", round(specificity, n),
                  "\nPositive Predictive Value = ", round(ppp, n),
                  "\nNegative Predictive Value = ", round(npp, n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep="")
  cat(result)
}


# Listing 17.9 - Performance of breast cancer data classifiers
#performance(dtree.perf)
#performance(ctree.perf)
#performance(forest.perf)
svm.perf
performance(svm.perf)


# Using Rattle Package for data mining

loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
ds <- "pima-indians-diabetes/pima-indians-diabetes.data"
url <- paste(loc, ds, sep="")
diabetes <- read.table(url, sep=",", header=FALSE)
names(diabetes) <- c("npregant", "plasma", "bp", "triceps",
                     "insulin", "bmi", "pedigree", "age", "class")
diabetes$class <- factor(diabetes$class, levels=c(0,1),
                         labels=c("normal", "diabetic"))
library(rattle)
rattle()



