#回归
attach(iris)
table(Species)
#typeof(iris)
iris
#change the column of “Species??? into clustering values from 0,1,2 ~,
iris$Species <- as.character((iris$Species))
iris$Species[iris$Species=='setosa'] <- 0
iris$Species[iris$Species=='versicolor']<-1
iris$Species[iris$Species=='virginica']<-2
typeof(Species)
#build a simple linear regression, using “Sepal.Length ??? as the independent variable,  and the “sepal.width ??? as the dependent variable
names(iris)
fun1<-lm(Sepal.Width ~ Sepal.Length,data=iris)
summary(fun1)

#build a polynomial regression using the same variables.
fun2<-lm(Sepal.Width ~ Sepal.Length+I(Sepal.Length^2),data=iris)

summary(fun2)
#4.make diagnostics and give the analysis.
plot(iris$Sepal.Length,iris$Sepal.Width,xlab = "Sepal.Length",ylab = "Sepal.Width")
#abline(fun1)
#lines(iris$Sepal.Length,fitted(fun2))
lines(fitted(fun1),col="blue",pch="22",lty=6)
lines(fitted(fun2),col="red",pch="22")
#abline(fun1)
#abline(fun2)#In abline(fun2) : 只用两个3回归系数中的第一???
#5.  check the correlation between every variable in the dataset
library(car)
iris$Species<-as.double(iris$Species)
irisData<-as.data.frame(iris[,c("Sepal.Length" ,"Sepal.Width" , "Petal.Length", "Petal.Width",  "Species")])
#typeof(Species)
cor(irisData)
scatterplotMatrix(irisData,spread = FALSE,smoother.args = list(lty=2),main="Scatter Plot Matrix")
#6. build a multiple linear regression using “Species??? as the dependent variable and the others as the 
#independent variables. Give explanation for the results.
fun3<-lm(Species ~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=irisData)
summary(fun3)

confint(fun3)

fun4<-lm(Species ~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width+Petal.Length:Petal.Width,data=irisData)
summary(fun4)
plot(fitted(fun4))
confint(fun4)

par(mfrow=c(2,2))
plot(fun3)

outlierTest(fun4)#离群???
hat.plot<-function(fun4){
  p<-length(coefficients(fun4))
  n<-length(fitted(fun4))
  plot(hatvalues(fun4),main = "Index Plot of Hat Values")
  abline(h=c(2,3)*p/n,col="red",lty=2)
  identify(1:n,hatvalues(fun4),names(hatvalues(fun4)))
}
hat.plot(fun4)
detach(iris)


#P163简单线性回归
#8-1
fit<-lm(weight ~ height,data=women)
summary(fit)
women$weight
fitted(fit)
plot(women$height,women$weight)
abline(fit)
#lines(women$height,fitted(fit))
#8-2
fit2<-lm(weight ~height+I(height^2),data=women)
summary(fit2)
plot(women$height,women$weight,xlab = "height",ylab ="weight",col="blue" )
lines(women$height,fitted(fit2),col="red")
abline(fit)
library(car)
scatterplot(weight~height,data=women,spread=FALSE,smoother.args=list(lty=2),pch=19,main="women age 30-39",xlab = "height",ylab = "weight")
#8-3
states <- as.data.frame(state.x77[, c("Murder", "Population","Illiteracy","Income","Frost")])
cor(states)
library(car)
scatterplotMatrix(states,spread=FALSE,smoother.args=list(lty=2),pch=19,main="scatter plot matrix")
#8-4
states <- as.data.frame(state.x77[, c("Murder", "Population","Illiteracy","Income","Frost")])
fit3<-lm(Murder~Population+Illiteracy+Income+Frost,data=states)
summary(fit3)
#confint(fit3)
par(mfrow=c(2,2))
plot(fit3)
#8-5
library(car)
cor(mtcars)
qqPlot(fit3,labels=row.names(states),id.method="identify",simulate=TRUE)
states["Nevada",]
fitted(fit3)["Nevada"]

residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}
residplot(fit3)
crPlots(fit3)
coefficients(fit3)
#hat statistic
hat.plot <- function(fit){
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main = "Index Plot of Hat Values")
  abline(h = c(2, 3) * p/n, col = "red", lty = 2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}

hat.plot(fit3)
row.names(states)
statesDel <- states[row.names(states) != 'Nevada',]
statesDel <- statesDel[row.names(statesDel) != 'Alaska',]
dim(states)
dim(statesDel)
fit4<-lm(Murder~Population+Illiteracy+Income+Frost,data=statesDel)
summary(fit4)
par(mfrow=c(2,2))
plot(fit4)
hat.plot(fit4)
influencePlot(fit4, id.method = "identify", main = "Influence Plot", 
              sub = "Circle size is proportial to Cook's Distance")
#not contain Income+Frost
fit5<-lm(Murder~Population+Illiteracy,data=statesDel)
summary(fit4)
anova(fit5,fit4)
AIC(fit4,fit5)
library(MASS)
fit6<-lm(Murder~Population+Illiteracy+Income+Frost,data=states)
stepAIC(fit6,direction = "backward")
#全子集回归
if(!require(leaps)){
  install.packages("leaps")
}
statesDel <- states[row.names(states) != 'Alaska',]
statesDel <- states[row.names(states) != 'Nevada',]
statesDel <- statesDel[row.names(statesDel) != 'Alaska',]
library(leaps)
leaps<-regsubsets(Murder~Population+Illiteracy+
                    Income+Frost,data=states,nbest = 4)
plot(leaps,scale = "adjr2")
library(car)
subsets(leaps,statistic="cp",main="Cp plot for all subsets regression")
abline(1,1,lty=2,col="red")
#交叉验证
# Listing 8.15 - Function for k-fold cross-validated R-square
shrinkage <- function(fit, k = 10) {
  if(!require(bootstrap)){
    install.packages("bootstrap")
  }
  require(bootstrap)
  # define functions
  theta.fit <- function(x, y) {
    lsfit(x, y)
  }
  theta.predict <- function(fit, x) {
    cbind(1, x) %*% fit$coef
  }
  
  # matrix of predictors
  x <- fit$model[, 2:ncol(fit$model)]
  # vector of predicted values
  y <- fit$model[, 1]
  
  results <- crossval(x, y, theta.fit, theta.predict, ngroup = k)
  r2 <- cor(y, fit$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2 - r2cv, "\n")
}
fit7<-lm(Murder~Population+Illiteracy+Income+Frost,data=states)
shrinkage(fit7)
fit8<-lm(Murder~Population+Illiteracy,data=states)
shrinkage(fit8)
fit9<-lm(Murder~Population+Illiteracy,data=statesDel)
shrinkage(fit9)
# Listing 8.16 - relweights() function for calculating relative
# importance of predictors

########################################################################
# The relweights function determines the relative importance of each   #
# independent variable to the dependent variable in an OLS regression. #
# The code is adapted from an SPSS program generously provided by      #
# Dr. Johnson.                                                         #
#                                                                      #
# See Johnson (2000, Multivariate Behavioral Research, 35, 1-19) for   #
# an explanation of how the relative weights are derived.              #
########################################################################
relweights <- function(fit, ...) {
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  
  # correlations between original predictors and new orthogonal variables
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda^2
  
  # regression coefficients of Y on orthogonal variables
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta^2)
  rawwgt <- lambdasq %*% beta^2
  import <- (rawwgt/rsquare) * 100
  lbls <- names(fit$model[2:nvar])
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  
  # plot results
  barplot(t(import), names.arg = lbls, ylab = "% of R-Square", 
          xlab = "Predictor Variables", main = "Relative Importance of Predictor Variables", 
          sub = paste("R-Square = ", round(rsquare, digits = 3)), 
          ...)
  return(import)
}
# using relweights()

fit10 <- lm(Murder ~ Population + Illiteracy + Income + 
            Frost, data = states)
relweights(fit10, col = "lightgrey")
relweights(fit10, col = "blue")
