setwd("otto group kaggle")
load("data_features back up.RData")

library(data.table)


# KNN=get.knn(data[,2:94,with=F],k=100)


train=subset(data,!is.na(data$target))
test=subset(data,is.na(data$target))

library(gbm)

param=c(ntree=40,depth=10,shrinkage=0.01,train.fraction=0.5,minsize=50)
modeldp=gbm(1*(factor(target)=="Class_1")~.-id,
            data=train[,1:95,with=F],
            n.trees = param[1],interaction.depth = param[2],
            shrinkage = param[3],train.fraction = param[4],n.minobsinnode=param[5],verbose = T)

summary(modeldp)


res=data.table(cbind(id=data$id,res))
res=subset(res,is.na(data$target))
write.table(res[,2:10,with=F],file="/Users/p-condemine/Downloads/submission-FNN.csv", sep = ",", col.names = NA,
          qmethod = "double")
# compter les occurences de chaque facteur par ligne...

plot(modeldp,i.var=96)


rowSums(res[,2:10,with=F])

# sample 10 variables in 93 and boostrap like this 100 or 1000 times to find 
# cluster with proportion of 1 class =100 and volume >100
deterministe=NULL
element=96
for (element in 96:107){
ratio=table(factor(data[[colnames(data)[element]]]),factor(data$target))
size=rowSums(ratio)
ratio=ratio/size
ratio=cbind(ratio,max_proportion=apply(ratio,1,max))
ratio=cbind(ratio,size=size)

ratio=subset(ratio,(ratio[,10]==1)&(ratio[,11]>50))
if(nrow(ratio)>0){
ratio=data.table(colnames(data)[element],ratio)
deterministe=rbind(deterministe,ratio)
}
}
