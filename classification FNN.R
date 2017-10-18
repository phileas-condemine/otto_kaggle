setwd("otto group kaggle")
load("data_features back up.RData")

library(data.table)
library(FNN)


# KNN=get.knn(data[,2:94,with=F],k=100)
temp=data.table(scale(data[,2:94,with=F],center = FALSE,scale = TRUE),target=factor(data$target))


train=subset(temp,!is.na(data$target))
test=subset(temp,is.na(data$target))

k=300

system.time(KNN<-get.knnx(data = train[,1:93,with=F],query = temp[,1:93,with=F],k=k))

votes=data.table(matrix(train$target[KNN$nn.index],ncol = k))
weights=data.table(matrix(exp(-KNN$nn.dist/KNN$nn.dist[,1]),ncol=k))
weights=weights/rowSums(weights)
quantile(x = weights$V1/weights$V200,probs = 0:100/100)
res=NULL
for (i in 1:9){
  res=data.table(cbind(res,x=rowSums((weights)*(votes==paste("Class_",i,sep = "",collapse = "")))))
  setnames(res,"x",paste("class_",i,sep="",collapse=""))
}

setnames(res,c("class_1","class_2","class_3","class_4","class_5","class_6","class_7","class_8","class_9"
               ),c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8",
                    "Class_9"))

res=data.table(cbind(id=data$id,res))
res=subset(res,is.na(data$target))



# just take the nearest of each class to compute proba
test=subset(data,is.na(data$target))
volumecat=summary(factor(data$target))
res=NULL
for(i in 1:9){
volume=volumecat[i]
num=floor(volume/500)
class=subset(data,data$target==paste("Class_",i,sep = "",collapse = ""))
KNN=get.knnx(data=class[,2:94,with=F],query=test[,2:94,with=F],k=num)
res=data.table(cbind(res,x=KNN$nn.dist[,num]))
setnames(res,"x",paste("Class_",i,sep="",collapse=""))
}
x=apply(res,1,min)
res2=exp(-res/x)
res2=res2/rowSums(res2)


write.table(res2,file="/Users/p-condemine/Downloads/submission-per500th-catNNexp.csv", sep = ",", col.names = NA,
          qmethod = "double")
# compter les occurences de chaque facteur par ligne...

rowSums(res[,2:10,with=F])

# sample 10 variables in 93 and boostrap like this 100 or 1000 times to find 
# cluster with proportion of 1 class =100 and volume >100





