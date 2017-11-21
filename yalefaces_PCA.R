library(jpeg)
library(reshape2)
library(psych)
path<-"E:/changeformat/"
fileNames<-list.files(path)
#��һ��ͼƬ������һ��
extract <- function(var){
  file<-paste(path,var,sep = "")#connect string#0�ڣ�255�ǰ�
  img<-readJPEG(file)#3ά��һ��
  img1<-img[,,1]
  dim(img1)
  imelt<-melt(img1)
  return(t(imelt[3]))
}
dim(img1)
#������һ��
result<-extract(fileNames[1])
for(i in 2:length(fileNames)){
  value<-extract(fileNames[i])
  #���ϳ�һ�����ݿ�
  result<-rbind(result,value)
  
}
dim(result)
class(resul)
res=result[,20061:70060]
dim(res)
for(j in 1:5000){
  if(j%%2==0){
    res=res[,-j]
  }
}
class(res)
cor(result)

fa.parallel(result,fa="pc",n.iter = 100,show.legend =TRUE,main="Scree plot with parellel analysis")
pc<-principal(result,nfactors = 2,rotate = "none")
pc
pc<-principal(result,nfactors = 2,rotate = "varimax" ,scores = TRUE)
pc
score<-pc$score

