
library(rpart)
library(mlbench)
data("Ionosphere")

#set seed to ensure reproducible results

set.seed(42)
#split into training and test sets
trainsetindex <- sample(1:nrow(Ionosphere),as.integer(0.80*nrow(Ionosphere)),replace=FALSE)

#separate training and test sets
trainset <- Ionosphere[trainsetindex,]
testset <- Ionosphere[-trainsetindex,]


#get column index of predicted variable in dataset

typeColNum <- grep("Class",names(Ionosphere))
#build model
rpart_model <- rpart(Class~.,data = trainset, method="class")

#plot tree

plot(rpart_model);
text(rpart_model)
####prediction
predict<-predict(rpart_model,testset[,-typeColNum],type="class")
######confusion matrix
table(true=testset$Class,pred=predict)
#######Accuracy of the model=0.8732394
mean(predict==testset$Class)
