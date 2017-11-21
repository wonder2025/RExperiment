library(grid)
library(vcd)
counts <- table(Arthritis$Improved)
counts
barplot(counts, main = "Simple Bar Plot", xlab = "Improvement", ylab = "Frequency",horiz = TRUE)
Arthritis
counts <- table(Arthritis$Improved, Arthritis$Treatment)
counts

barplot(counts, main = "Stacked Bar Plot", xlab = "Treatment", 
        ylab = "Frequency", col = c("red", "yellow", "green"), 
        legend = rownames(counts))
barplot(counts, main = "Stacked Bar Plot", xlab = "Treatment", 
        ylab = "Frequency", col = c("red", "yellow", "green"), 
        legend = rownames(counts))
opar <- par(no.readonly=TRUE) # record current settings
attach(mtcars)                                                     
plot(wt, mpg, 
     main="Basic Scatterplot of MPG vs. Weight",       
     xlab="Car Weight (lbs/1000)", 
     ylab="Miles Per Gallon ", pch=19)
abline(lm(mpg ~ wt), col="red", lwd=2, lty=1)            
lines(lowess(wt, mpg), col="blue", lwd=2, lty=2)  

set.seed(1234)
n <- 10000
c1 <- matrix(rnorm(n, mean=0, sd=.5), ncol=2)
c2 <- matrix(rnorm(n, mean=3, sd=2), ncol=2)
mydata <- rbind(c1, c2)
mydata <- as.data.frame(mydata)
names(mydata) <- c("x", "y")
with(mydata, plot(x, y, pch=19, main="Scatter Plot with 10000 Observations"))
with(mydata,
     smoothScatter(x, y, main="Scatterplot colored by Smoothed Densities"))

boxplot(mpg~cyl,data=mtcars)
