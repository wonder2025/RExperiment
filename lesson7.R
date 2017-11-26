adenocarcinoma=read.csv("F:/R/1_Dataset/adenocarcinoma.csv",header = TRUE)
adenocarcinoma_2_11=adenocarcinoma[2:11]
head(adenocarcinoma_2_11)
library(psych)
fa.parallel(adenocarcinoma_2_11,fa="pc",n.iter = 100,show.legend =FALSE,main="Scree plot with parellel analysis")
pc<-principal(adenocarcinoma_2_11,nfactors = 2,rotate = "none")
pc
pc<-principal(adenocarcinoma_2_11,nfactors = 2,rotate = "varimax" ,scores = TRUE)
pc
pc$score


#######################考试#######################
if(!require(psych)){
  install.packages("psych")
}
require(psych)

class(Harman23.cor)
dim(Harman23.cor$center)
fa.parallel(Harman23.cor$cov,n.obs = 302,fa="pc",n.iter = 100,
            show.legend=TRUE,main ="scree plot with parellel analysis")
abline(h=1,lwd=1,col="grey")#添加一条水平直线y=1，线宽为4

#14-2
pc<-principal(Harman23.cor$cov,nfactors = 2,rotate = "none")
pc
#14-3
pc<-principal(Harman23.cor$cov,nfactors = 2,rotate = "varimax")
pc
