library(R.matlab)
library(psych)
#path <- system.file("mat-files", package="R.matlab")
pathname <- file.path(getwd(), "Yale_32x32.mat")
data <- readMat(pathname)
fea<-data$fea
gnd<-data$gnd
dim(fea)
dim(gnd)
#画图
aa<-fea[23,]
typeof(aa)
im<-matrix(aa,32,32)
image(im)


#只取前面33张脸做PCA
new_data<-fea[1:33,]
#归一化
#new_data.scale<-scale(new_data)
new_data.scale<-new_data
dim(new_data.scale)#33*1024
fa.parallel(new_data.scale,fa="pc",n.iter = 100,
            show.legend =TRUE,main="Scree plot with parellel analysis")
abline(h=1)
pc<-principal(new_data.scale,nfactors = 7 ,rotate="none",sorce="true")
pc
#主成分旋转,结果表明不用旋转
#pc<-principal(new_data.scale,nfactors = 7 ,rotate="varimax")
#pc
#kmeans
dim(pc$scores)#33*7
if(!require(NbClust)){
  install.packages("NbClust")
}else{
  require(NbClust)
}
library(caret)
install.packages("clv")
library(clv)
#得到分类标签
gnd1<-gnd[1:33,]
#选择聚类中心点
nc<-NbClust(pc$scores,min.nc = 2,max.nc = 10,method = "kmeans")
table(nc$Best.n[1,])

#聚类函数
fit.km<-kmeans(pc$scores,3,nstart = 13)
fitted(fit.km)
#remove(gnd)
ct.km<-table(gnd1,fit.km$cluster)
ct.km
#library(flexclust)
#randIndex1<-randIndex(ct.km,correct = FALSE)#0.161
#randIndex1

set.seed(1234)
a<-NULL
res<-NULL
rand_inds<-NULL
len<-c(1:30)
for(j in len){
  fit.km<-kmeans(pc$scores,6,nstart =j)
  #混淆矩阵
 # covtypeConfusionMatrix <- confusionMatrix(fit.km$cluster, gnd1)

  #accuracy
 # a[j]<-  covtypeConfusionMatrix$overall[1]
  res <-cbind(res,fit.km$cluster)
  #兰德系数
  std <- std.ext(fit.km$cluster,gnd1)
  rand_inds[j] <- clv.Rand(std)   #Rand Index
    
}

max(rand_inds)

plot(len,a)
#准确率最大值时j的值
index<-which(a==max(a), arr.ind = TRUE)
index<-which(rand_inds==max(rand_inds), arr.ind = TRUE)
plot(len,rand_inds)
max(rand_inds)
covtypeConfusionMatrix <- confusionMatrix(res[,6], gnd1)
covtypeConfusionMatrix
covtypeConfusionMatrix$overall[1]

for(i in res) {
  std <- std.ext(res[,3],gnd1)
  rand_inds[j] <- clv.Rand(std)   #Rand Index

}
