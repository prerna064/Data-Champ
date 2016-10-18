setwd("C:\\PERSONAL\\edureka\\Module-3\\R")
ins<-read.table("clust.csv",head=T,sep=",")
head(ins,10)
ins1<-ins[,c(2,5)]
head(ins1)
km<-kmeans(ins1,4)

km$cluster
km$centers
km$totss
km$withinss
km$tot.withinss
km$betweenss
km$size

km$cluster

ins2<-cbind(ins1,km$cluster)
head(ins2)
plot(ins2$Income,ins2$Age,col=km$cluster)

write.csv(ins2,"Output.csv")

