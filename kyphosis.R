rm(list = ls())

library(rpart)
library(clv)

test_data <- kyphosis
real_cluster <- as.numeric(test_data[,1])
cluster_num <- length(unique(real_cluster)) #得到clusters的个数，即centers的个数
test_data <- test_data[,-1]  #剔除类别
mykmeans = kmeans(test_data, cluster_num)
std <- std.ext(mykmeans$cluster, as.integer(real_cluster))
rand_ind <- clv.Rand(std)   #Rand Index
rand_ind