x<-rnorm(1000,2,6)
ll<-function(theta,data){
  n<-length(data)
  ll<--0.5*n*log(2*pi)-0.5*n*log(theta[2])-1/2/theta[2]*sum((data-theta[1])^2)
  return(-ll)
}
nlminb(c(0.5,2),ll,data=x,lower=c(-100,0),upper=c(100,100)) $par

#-------------------------------------------------
x<-rpois(100,2)
sum(x)
f<-function(lambda)lambda^(215)*exp(10*lambda)/(1.580298*10^51)
optimize(f,c(1,3),maximum=T)

#------------------------------------------------
#读入数据
library(rJava)
library(xlsxjars)
library(xlsx)
csv=read.csv("F:/R/作业/Lung_cancer.csv",header = TRUE)
#dim(csv)
#head(csv)
csv_2_3=csv[,2:3]
csv_2=csv[,2]
csv_3=csv[,3]
mean(csv_2)
median(csv_2)
var(csv_2)
mad(csv_2)
quantile(csv_2,0.3,0.8)
range(csv_2)
sum(csv_2)
diff(csv_2)
min(csv_2)
max(csv_2)
scale(csv_2)
#print(csv_2_3[1,1])
#计算协方差
cov(csv_2,csv_3)
#生成一个1000个数据的正态分布，自己随机的设置均值和方差
myNormalData<-rnorm(1000,0,3)
help(rnorm)
hist(myNormalData)
#使用矩估计方法，反过来把这个正态分布数组的点估计求出来，看看和设置的是否一样
ll<-function(theta,data){
  n<-length(data)
  ll<--0.5*n*log(2*pi)-0.5*n*log(theta[2])-1/2/theta[2]*sum((data-theta[1])^2)
  return(-ll)
}
nlminb(c(0.5,2),ll,data=myNormalData,lower=c(-100,0),upper=c(100,100)) $par
#[1] 0.07133034 9.36458128


#依照书中表5.4介绍的函数，生成一个100个数据的泊松分布，随机设置参数；
help(rpois)
randPois<-rpois(1000,lambda = 2)
hist(randPois)
#使用极大似然估计求参数，重点学习optimize这个函数；
#optimize函数求给定区间[a,b]中一元函数f的最值
#在对多元函数求极值时，则需利用到更强大的optim函数，
help(optimize)
x<-function(a){sin(a)}
optimize(x,c(-1,1),maximum = TRUE)


poisEstimate <- function(lambda)lambda^(215)*exp(10*lambda)/(1.580298*10^51)

optimize(poisEstimate, c(1,3), maximum=T)
#$maximum
#[1] 2.999959

#$objective
#[1] 2.568691e+64


# 使用课件中给出的方法，需要练习编程的同学，写一个function，精力有限的同学使用课件
#     中的function，对Lung_cancer文件的第二和第三列分别给出均值95%和90%的置信区间，比
#     较其中的区别，写出分析；
help(t.test)
t.test(csv_2, y = NULL, 
       alternative = c("two.sided", "less","greater"), 
       mu = 0, paired = FALSE, var.equal = FALSE, 
       conf.level = 0.95)

t.test(csv_2, y = NULL, 
       alternative = c("two.sided", "less","greater"), 
       mu = 0, paired = FALSE, var.equal = FALSE, 
       conf.level = 0.90)
