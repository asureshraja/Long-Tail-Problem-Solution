require(RWeka)
print("Loading Instances....")
dataset <- read.arff("C:\\Users\\Suresh\\Desktop\\rbmproject\\datasets\\formattedfulldataset.arff")
print("Load Completed")
plot(0,0,xlim=c(1,2077),ylim=c(0,2),pch=20,main="Each Item Method",xlab="Num of Ratings",ylab="Rmse")
choice<-2

uptoid=1682
i=1
maes<-c()
rmses<-c()
ids<-c()
nums<-c()
while(i<=uptoid){
print(sprintf("working with %d ",i))
traintestset<-dataset[dataset$movieid==i,]
if(length(traintestset[,1])<10){
i=i+1
next
}
if(choice==1){
algo<-LinearRegression(rating~age+gender+occupation+c_avg_rat,data=traintestset)
}else if(choice==2){
algo<-Bagging(rating~age+gender+occupation+c_avg_rat,data=traintestset)
}else if(choice==3){
algo<-DecisionStump(rating~age+gender+occupation+c_avg_rat,data=traintestset)
}else if(choice==4){
algo<-M5Rules(rating~age+gender+occupation+c_avg_rat,data=traintestset)
}else{
stop("failed to choose algo")
}
e<-evaluate_Weka_classifier(algo,numFolds = 10)
lengthofset<-length(traintestset[,1])
print(lengthofset)
nums<-c(nums,lengthofset)
ids<-c(ids,i)
print(e$details[2])
maes<-c(maes,e$details[2])
#points(i,e$details[2],col="blue")
points(lengthofset,e$details[2],col="blue",pch=20)
rmses<-c(rmses,e$details[3])
print(e$details[3])
i=i+1
}


output<-data.frame(ids,maes,rmses,nums)
write.csv(output,paste("eiset1op",as.character(choice),".csv",sep=""),row.names=FALSE)