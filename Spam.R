require(rpart)
library(party)
setwd("C:/Users/navin/Desktop/Edureka/module4")
spam.data <- read.csv("spambase.data",header=F)
names(spam.data)[58]="Spam"

set.seed(42)
#split into training and test sets
trainsetindex <- sample(1:nrow(spam.data),as.integer(0.80*nrow(spam.data)),replace=FALSE)

#separate training and test sets
trainset <- spam.data[trainsetindex,]
testset <- spam.data[-trainsetindex,]


typeColNum <- grep("Spam",names(spam.data))
#build model
rpart_model <- rpart(Spam~.,data = trainset, method="class")

#plot tree
plot(rpart_model)


#…and the moment of reckoning

rpart_predict <- predict(rpart_model,testset[,-typeColNum],type="class")


#confusion matrix
table(rpart_predict,true=testset$Spam)