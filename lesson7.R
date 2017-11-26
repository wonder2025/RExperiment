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