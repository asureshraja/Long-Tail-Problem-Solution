require(RWeka)
print("Loading Instances....")
dataset <- read.arff("set1.arff")
clusterdataset <- read.arff("tcmovies.arff")
print("Load Completed")
plot(0,0,xlim=c(1,2077),ylim=c(0,2),pch=20,main="Total Clustering Method",xlab="Num of Ratings",ylab="Rmse")
choice<-1
size=100
skm<-SimpleKMeans(clusterdataset,Weka_control(N = 100))
uptoid=3951
i=1
maes<-c()
rmses<-c()
ids<-c()
while(i<=size){
print(sprintf("working with cluster %d ",i))

#temp<-clusterdataset[which(skm$cluster==i),][,1]
temp<-predict(skm)
temp1<-clusterdataset[which(temp==i),][,1]
traintestset<-dataset[dataset$movieid %in% temp1,]
print(length(traintestset[,1]))
if(length(traintestset[,1])<10){
i=i+1
next
}
if(choice==1){
algo<-LinearRegression(rating~age+gender+occupation+c_avg_rat+c_qty,data=traintestset)
}else if(choice==2){
algo<-Bagging(rating~age+gender+occupation+c_avg_rat+c_qty,data=traintestset)
}else if(choice==3){
algo<-DecisionStump(rating~age+gender+occupation+c_avg_rat+c_qty,data=traintestset)
}else if(choice==4){
algo<-M5Rules(rating~age+gender+occupation+c_avg_rat+c_qty,data=traintestset)
}else{
stop("failed to choose algo")
}
e<-evaluate_Weka_classifier(algo,numFolds = 10)
ids<-c(ids,i)
maes<-c(maes,e$details[2])
points(i,e$details[2],col="blue")
rmses<-c(rmses,e$details[3])
i=i+1
}


plot(0,0,xlim=c(1,2077),ylim=c(0,2),pch=20,main="Total Clustering Method",xlab="Num of Ratings",ylab="Rmse",col="blue")

i=1
j=1
while(i<=size){
val<-length(which(temp==i))
print(val)
j=1
while(j<=val){
print(nrow(dataset[dataset$movieid==which(temp==i)[j],]))
#points(which(temp==i)[j],rmses[i])
points(nrow(dataset[dataset$movieid==which(temp==i)[j],]),rmses[i],pch=20,col="blue")

j=j+1
}
i=i+1
}

output<-data.frame(ids,maes,rmses)
write.csv(output,paste("tcset1op",as.character(choice),".csv",sep=""),row.names=FALSE)