setwd("otto group kaggle")
library(data.table)


load("data_features.RData")
train=subset(data,!is.na(data$target))
test=subset(data,is.na(data$target))

# unsupervised -> keep clusters with > 90% (X%) 1-2 values

library(Rclusterpp)
library(ggdendro)
for (nb in c(5,10,20)){
cluster=FactoMineR::PCA(data[,2:94,with=F],ncp = nb)
dimred=data.table(cluster$ind$coord)
for (method in c("ward","single","complete","average")){
  for(distance in c("euclidean", "manhattan", "maximum","minkowski")){
    system.time(clust<-Rclusterpp.hclust(dimred,method=method,distance=distance))
for (k in c(9,20,50,100,200)){
data$cluster_PCA=cutree(tree = clust,k = k)
setnames(data,"cluster_PCA",paste("cluster_PCA",nb,"_k",k,
                                  "_",method,"_",distance,sep="",collapse=""))
}
save(list = "data",file = "data_features.RData")
}
}
}
save(list = "data",file = "data_features back up.RData.RData")





ratio=table(factor(data$cluster_PCA50_k50),factor(data$target))
size=rowSums(ratio)
ratio=ratio/size
ratio=cbind(ratio,max_proportion=apply(ratio,1,max))
ratio=cbind(ratio,size=size)
ratio

# go to the cluster with the best ratio

# check FNN et kmeans pour creer des features de cluster ou simplement des candidats.

library(FNN)


save(list = "data",file = "data_features back up.RData")
