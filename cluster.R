
library(flexclust)
data(nutrient,package="flexclust")
head(nutrient,4)
d<-dist(nutrient)
d
class(d)
as.matrix(d)[1:4,1:4]



data(nutrient, package="flexclust")
nutrient
row.names(nutrient) <- tolower(row.names(nutrient))
nutrient.scaled <- scale(nutrient)   
nutrient.scaled
d <- dist(nutrient.scaled)                                          
fit.average <- hclust(d, method="average")                          
plot(fit.average, hang=-1, cex=.8, main="Average Linkage Clustering")


library(NbClust)
nc <- NbClust(nutrient.scaled, distance="euclidean", 
              min.nc=2, max.nc=15, method="average")
par(opar)
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria") 

library(NbClust)
nc <- NbClust(nutrient.scaled, distance="euclidean", 
              min.nc=2, max.nc=15, method="average")
par(opar)
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria") 

library(rattle)
library(rJava)
library(xlsxjars)
library(xlsx)
winea<-read.xlsx("F:/R/wine01.xlsx",1)
winea
# Plot function for within groups sum of squares by number of clusters
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
class(winea)
wine<-as.matrix(winea)
df<-scale(wine[-1])

wssplot(df)      
library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
par(opar)
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria") 
set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25) 
fit.km$size
fit.km$centers                                               
aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

#####################assignment####层次聚类##################################
library(rpart)
library(NbClust)
kyphosis
a<-kyphosis[-1]
summary(kyphosis)
kyphosis.scale<-a
kyphosis.scale<-scale(a)
kyphosis.scale
d<-dist(kyphosis.scale)
fit.average<-hclust(d,method = "average")
plot(fit.average,main="average",hang=-1)

#fit.average<-hclust(d,method = "centroid")
#plot(fit.average,main="centroid", hang=-1)
nc<-NbClust(kyphosis.scale,distance = "euclidean",min.nc = 2,max.nc = 15,method="average")
nc$Best.n
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),xlab="Number of Clusters",ylab="Number of Criteria")
fit.average
clusters<-cutree(fit.average,k=2)
clusters
aggregate(a,by=list(cluster=clusters),median)
aggregate(as.data.frame(kyphosis.scale),by=list(cluster=clusters),median)

plot(fit.average,hang=-1,cex=.8,main="2 cluster")
rect.hclust(fit.average,k=2)
type<-as.integer(kyphosis$Kyphosis)
#type
ct.km<-table(type,clusters)
randIndex2<-randIndex(ct.km)#兰德指数为0.161
randIndex2
##########################k-means#############################
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(kyphosis.scale)
set.seed(1234)
nc<-NbClust(kyphosis.scale,distance = "euclidean",min.nc = 2,max.nc = 15,method="average")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),xlab="Number of Clusters",ylab="Number of Criteria")

set.seed(1234)

fit.km<-kmeans(kyphosis.scale,2,nstart = 1000)

fit.km$size
#class(fit.km$cluster)

type<-as.integer(kyphosis$Kyphosis)
#type
ct.km<-table(type,fit.km$cluster)
kyphosis$Kyphosis
fit.km$cluster
fit.km$cluster
library(flexclust)

randIndex1<-randIndex(ct.km)#0.161
randIndex1
###############不同的包进行评价#####################
library(clv)
std <- std.ext(fit.km$cluster,type)
rand_ind <- clv.Rand(std)   #Rand Index
rand_ind#0.5882716

#################wine#########################
library(xlsxjars)
library(xlsx)
library(psych)
library(flexclust)
library(NbClust)
winea<-read.xlsx("D:/深圳培训/R/wine01.xlsx",1)
winea
dim(winea)
###########wine###主成分分析####################
type<-as.integer(factor(winea$Type))
type
wine_scale<-scale(winea[-1])
fa.parallel(wine_scale,fa="pc",n.iter = 100,show.legend = FALSE,main = "parallel analysis" )
pc<-principal(wine_scale,nfactors = 3,rotate="none",sorce="true")

pc$loadings
dim(pc$weights)
new_data<-pc$scores
###########wine###因子分析EFA####################

#wine_cor<-cor(winea[-1])
wine_cor<-winea[-1]
fa.parallel(wine_cor, n.obs=112, fa="fa", n.iter=100,show.legend=FALSE)
fa <-fa(wine_cor,nfactors=2,rotate="varimax",fm="pa" )
factor.plot(fa,labels=rownames(fa$loadings))
###################k-means##############################
#new_data<-winea
#set.seed(1234)
#####how many clusters###
nc<-NbClust(new_data,distance = "euclidean",min.nc = 2,max.nc = 15,method="average")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),xlab="Number of Clusters",ylab="Number of Criteria")
#wine.km<-kmeans(new_data,2,nstart = 100)
wine.km<-kmeans(new_data,3,nstart = 100)
wine.km$size
wine.kmm<-table(winea$Type,wine.km$cluster)

randIndex1<-randIndex(wine.kmm,correct = FALSE)
randIndex1
m<-cbind(winea,wine.km$cluster)
m
m###############层次聚类##Hierarchical Clustering##########
#new_data<-winea
dist.e<-dist(new_data,method='euclidean')
heatmap(as.matrix(dist.e),labRow = F, labCol = F)
model1<-hclust(dist.e,method = "average")
result<-cutree(model1,k=3)
result
plot(model1,hang=-1,cex=.8,main="2 cluster")
rect.hclust(model1,k=3)
#type
class(winea)
winea$Type
ct.km<-table(winea$Type,result)
randIndex2<-randIndex(ct.km,correct = FALSE)
randIndex2






