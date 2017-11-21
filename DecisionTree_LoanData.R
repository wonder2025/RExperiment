# Classification with rpart package - Loan Data 
#############################################################
#install and load packages: rpart, rpart.plot

##############################################################
library(rpart)
library(rpart.plot)
library(ggplot2)
### Load data
loan3000 <- read.csv("F:/R/作业/decision tree/loan3000.csv", header = TRUE)

dim(loan3000)
summary(loan3000)
head(loan3000)
barchart(loan3000$outcome)
################################################################

loan_tree <- rpart(formula = outcome ~ borrower_score + payment_inc_ratio,
                   data=loan3000, 
                   control = rpart.control(cp=.005))


par(mar=c(0,0,0,0)+.1)
plot(loan_tree, uniform=TRUE, margin=.05)
text(loan_tree, cex=.75)
loan_tree
rpart.plot(loan_tree)
###############################################################################
#This is to illustrate how the algorithm works on loan data 
# Run the code to see a view of partition rules
# you don't need to understand this code now

r_tree <- data.frame(x1 = c(0.525, 0,     0.375, 0.525, 0.625),
                     x2 = c(0.525, 0.525, 0.375, 1,     0.625),
                     y1 = c(0,     9.732, 0,     8.772, 8.772),
                     y2 = c(25,    9.732, 9.732, 8.772, 25),
                     rule_number = factor(c(1, 2, 4, 3, 5)))

labs <- data.frame(x=c(.375/2, .45, 1.525/2, 1.625/2, .575, .525/2),
                   y=c(8.772/2, 8.772/2, 9.732/2, 
                       9.732 + (25 - 9.732)/2, 9.732 + (25 - 9.732)/2, 9.732 + (25 - 8.772)/2),
                   decision = factor(c('default', 'paid off', 'paid off', 'paid off', 'default', 'default')))



ggplot(data=loan3000, aes(x=borrower_score, y=payment_inc_ratio)) +
  geom_point( aes(color=outcome, shape=outcome), alpha=.5) +
  scale_color_manual(values=c('blue', 'red')) +
  scale_shape_manual(values = c(1, 2)) +
  # scale_shape_discrete(solid=FALSE) +
  geom_segment(data=r_tree, aes(x=x1, y=y1, xend=x2, yend=y2, linetype=rule_number), size=1.5, alpha=.7) +
  guides(colour = guide_legend(override.aes = list(size=1.5)),
         linetype = guide_legend(keywidth=3, override.aes = list(size=1))) +
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0), limits=c(0, 25)) + 
  geom_label(data=labs, aes(x=x, y=y, label=decision)) +
  #theme(legend.position='bottom') +
  theme_bw()

###############################################################################

## Gini coefficient and impurity

info <- function(x){
  info <- ifelse(x==0, 0, -x * log2(x) - (1-x) * log2(1-x))
  return(info)
}
x <- 0:50/100
plot(x, info(x) + info(1-x))

gini <- function(x){
  return(x * (1-x))
}
plot(x, gini(x))

impure <- data.frame(p = rep(x, 3),
                     impurity = c(2*x,
                                  gini(x)/gini(.5)*info(.5),
                                  info(x)),
                     type = rep(c('Accuracy', 'Gini', 'Entropy'), rep(51,3)))

ggplot(data=impure, aes(x=p, y=impurity, linetype=type, color=type)) + 
  geom_line(size=1.5) +
  guides( linetype = guide_legend( keywidth=3, override.aes = list(size=1))) +
  scale_x_continuous(expand=c(0,0.01)) + 
  scale_y_continuous(expand=c(0,0.01)) + 
  theme_bw() +
  xlab("p: proportion of misclassified records")+
  ylab("impurity")+
  theme( legend.title=element_blank()) 

###############################################################################
# divide data into two subsets forn training and testing
loanTrain <- loan3000[1:2000,]
dim(loanTrain)
loanTest <- loan3000[2001:3000,]
dim(loanTest)

# build tree from train data
#loan_tree1 <- rpart(formula = outcome ~ borrower_score + payment_inc_ratio,
#                   data=loanTrain, 
#                   control = rpart.control(cp=.005))
loan_tree1 <- rpart(formula = outcome ~ borrower_score + payment_inc_ratio,
                    data=loanTrain, 
                    control = rpart.control(cp=0.005))

par(mar=c(0,0,0,0)+.1)
plot(loan_tree1, uniform=TRUE, margin=.05)
text(loan_tree1, cex=.75)
plotcp(loan_tree1)
# predict on test data
loanPredict <- predict(loan_tree1,
                          newdata=loanTest,
                          type='class'
)
class(loanPredict)
head(loanPredict)
tail(loanPredict)

#evaluate
## make a contingency table 
loanTestTable <- table(loanPredict, loanTest$outcome)
loanTestTable
library(caret)
confusi<-confusionMatrix(loanPredict,loanTest$outcome)
confusi
# how to calculate the accuracy?

#####################################################################
#CP
loan_tree2 <- rpart(formula = outcome ~ borrower_score + payment_inc_ratio,
                    data=loanTrain, 
                    control = rpart.control(cp=0))
loan_tree2$control
rpart.plot(loan_tree2)
plotcp(loan_tree2)
printcp(loan_tree2)

##################################################################
# prune the tree 
prunetree<-prune(loan_tree2,cp=0.005)
rpart.plot(prunetree)
