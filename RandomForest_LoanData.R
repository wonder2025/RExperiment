###############################################################################
# ensemble models: random forest with Loan data随机森林无测试数据集
###############################################################################
#install and load package: randomForest, ggplot2
library(randomForest)
library(ggplot2)
###############################################################################
### Load data
loan3000 <- read.csv("F:/R/作业/decision tree/loan3000.csv", header = TRUE)

##############################################################################
#split
loanTrain <- loan3000[1:2000,]
loanTest <- loan3000[2001:3000,]
head(loanTest)
###############################################################################
#build a random forest
?randomForest

loanRF <- randomForest(outcome ~ borrower_score + payment_inc_ratio,
                   data=loanTrain,
                   ntree = 10,
                   importance = TRUE)
loanRF
loanRF$votes
loanRF$confusion
?randomForest
#error rate of random forest
# the Out-Of-Bag (OOB) estimate of error is the error rate for the trained models (i.e., trees), applied on 
# the data left out of the training set for that tree.
# the OOB error can be plotted versus the number of the trees in the random forest:
error_df = data.frame(error_rate = loanRF$err.rate[,'OOB'],
                      num_trees = 1:loanRF$ntree)
#this plot shows the improvement in accuracy of the random forest with the addition  of more trees
ggplot(error_df, aes(x=num_trees, y=error_rate)) +
  geom_line()  +
  theme_bw()

###############################################################################
# predict 
loanRFPredict <- predict(loanRF,
                         newdata = loanTest)
class(loanRFPredict)
head(loanRFPredict)
## plot of random forest predictions
loanRF_df <- cbind(loanTest, pred = loanRFPredict)

ggplot(data=loanRF_df, aes(x=borrower_score, y=payment_inc_ratio, 
                       shape=pred, color=pred)) +
  geom_point(alpha=.6, size=2) +
  scale_shape_manual( values=c( 1, 4)) +
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0), lim=c(0, 20)) + 
  theme_bw()
dev.off()
###############################################################################
#confusion matrix
loanRFPredictConf <- table(loanRFPredict, loanTest$outcome)
loanRFPredictConf

###############################################################################
# using ROCR package to check the performance 
# loanRFProb = predict(loanRF,type="prob",newdata=loanTest)[,2]
# loanRFPred = prediction(loanRFProb, loanTest$outcome)
# loanRFPredictPerf = performance(loanRFPred,"tpr","fpr")
# plot(loanRFPredictPerf,main="ROC Curve for Loan RF",col=2,lwd=2)
# abline(a=0,b=1,lwd=2,lty=2,col="gray")

#####越大越重要##gini越小越好########################################################################
#variable importance
varImpPlot(loanRF)

###############################################################################
## Fit random forest to all the loan data with more variables 
loan_data <- read.csv("E:/RExp/loan_data.csv", header = TRUE)
loan_data <- loan_data[,-c(1,2,15)]
dim(loan_data)
head(loan_data)

loanRF_all <- randomForest(outcome ~ ., 
                           data=loan_data,
                           importance=TRUE,
                           ntree = 50)
loanRF_all
?randomForest
#variable importance 
# type1: accuracy decrease
#The more the accuracy of the random forest decreases due to the exclusion (or permutation) of a single variable,
#the more important that variable is deemed, and therefore variables with a large mean decrease in accuracy are more important
# for classification of the data.比下面更准确，可用于特征选择
varImpPlot(loanRF_all, sort = TRUE, type=1)

#type2: Gini decrease
varImpPlot(loanRF_all, sort = TRUE, type=2)

imp1 <- importance(loanRF_all, type=1)
imp1
imp2 <- importance(loanRF_all, type=2)
imp2

# ## Variable importance plot for random forest
# this is an enhanced plots. in both types, variables are ranked by the decrease in accuracy.
idx <- order(imp1[,1])
nms <- factor(row.names(imp1)[idx], levels=row.names(imp1)[idx])
imp <- data.frame(Predictor = rep(nms, 2),
                  Importance = c(imp1[idx, 1], imp2[idx, 1]),
              Type = rep( c('Accuracy Decrease', 'Gini Decrease'), rep(nrow(imp1), 2)))

ggplot(imp) +
  geom_point(aes(y=Predictor, x=Importance), size=2, stat="identity") +
  facet_wrap(~Type, ncol=1, scales="free_x") +
  theme(
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line(linetype=3, color="darkgray") ) +
  theme_bw()

#####################################################################
#RF tuning
?tuneRF
#Add trees to an ensemble
?grow
