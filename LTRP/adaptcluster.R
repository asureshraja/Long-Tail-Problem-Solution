require(RWeka)
require(FNN)
print("Loading Instances....")
dataset <- read.arff("C:\\Users\\Suresh\\Desktop\\rbmproject\\datasets\\formattedfulldataset.arff")
knndataset <- data.matrix(read.csv("C:\\Users\\Suresh\\Desktop\\rbmproject\\datasets\\allitems.csv"))
knndataset<-replace(knndataset, is.na(knndataset), 0)
print("Load Completed")
get.knn( knndataset[,1:3],k=3)$nn.index[1,]
alpha=200
plot(0,0,xlim=c(1,2077),ylim=c(0,2),pch=20,main="Adaptive Clustering Method",xlab="Num of Ratings",ylab="Rmse")

choice<-1
size<-alpha
uptoid=1682
i=1
j=1
maes<-c()
rmses<-c()
ids<-c()
nums<-c()
while(i<=uptoid){
print(sprintf("working with %d ",i))
traintestset<-dataset[dataset$movieid==i,]
j=1
while(length(traintestset[,1])<alpha){
tempset<-dataset[dataset$movieid==get.knn(knndataset, k=size)$nn.index[1,][j],]
traintestset= rbind(traintestset,tempset)
j=j+1
}
print(length(traintestset[,1]))
if(choice==1){
algo<-LinearRegression(rating~age+gender+occupation+c_avg_rat,data=traintestset)
}else if(choice==2){
algo<-Bagging(rating~age+gender+occupation+c_avg_rat+c_qty,data=traintestset)
}else if(choice==3){
algo<-DecisionStump(rating~age+gender+occupation+c_avg_rat+c_qty,data=traintestset)
}else if(choice==4){
algo<-M5Rules(rating~age+gender+occupation+c_avg_rat+c_qty,data=traintestset)
}else{
stop("failed to choose algo")
}
e<-evaluate_Weka_classifier(algo,numFolds = 5)
ids<-c(ids,i)
lengthofmovierecord<-nrow(dataset[dataset$movieid==i,])
nums<-c(nums,lengthofmovierecord)
print(e$details[2])
maes<-c(maes,e$details[2])
#points(i,e$details[2],col="blue")
points(lengthofmovierecord,e$details[2],col="blue",pch=20)

rmses<-c(rmses,e$details[3])
print(e$details[3])
i=i+1
}
output<-data.frame(ids,maes,rmses,nums)
write.csv(output,paste("acset1op",as.character(choice),".csv",sep=""),row.names=FALSE)
