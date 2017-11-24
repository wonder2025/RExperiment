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
fit<-lm(weights = height,data=women)
summary(fit)
