library(R.matlab)
library(psych)
#path <- system.file("mat-files", package="R.matlab")
path<-"F:/R/yale人脸origin数据录入/Yale_15cl_1024f_165r_1stCLabel_20160714034545_561caca94011413bbf6a2b59839ad774/Yale_15cl_1024f_165r_1stCLabel"
pathname <- file.path(path, "Yale_32x32.mat")
data <- readMat(pathname)
fea<-data$fea
gnd<-data$gnd
dim(fea)
dim(gnd)
#画图
aa<-fea[23,]
image(aa)
im<-matrix(aa,32,32)
image(im)


#只取前面33张脸做PCA
new_data<-fea[1:33,]
new_data.scale<-scale(new_data)
fa.parallel(new_data.scale,fa="pc",n.iter = 100,show.legend =TRUE,main="Scree plot with parellel analysis")
pc<-principal(new_data.scale,nfactors = 3,rotate="none",sorce="true")
pc
#kmeans
fit.km<-kmeans(pc$scores,3,nstart = 100)
gnd<-gnd[1:33,]
ct.km<-table(gnd,fit.km$cluster)
library(flexclust)
randIndex1<-randIndex(ct.km,correct = FALSE)#0.161
randIndex1
