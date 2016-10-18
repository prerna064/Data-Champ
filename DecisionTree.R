install.packages("tree")
library(tree)
install.packages("party")
library(party)
data(iris)
View(iris)
colnames(iris)
##### creating tree using tree package###########
mytree<-tree(Species~Sepal.Width+Petal.Width,data=iris)
plot(mytree)
text(mytree)
############# Prediction using R
dim(iris)
dim(iris)[2]
######## create training data for 100 records and test data for 50 records
trainset<-sample(1:nrow(iris),as.integer(nrow(iris)/3),replace=FALSE)
test<-iris[trainset,]
train<-iris[-trainset,]
library(tree)
mytree<-tree(Species~ Sepal.Width+Petal.Width, data=train)##building model
mytree
x<-test[,c("Sepal.Width","Petal.Width")]
test$predictedClass<- predict(mytree,newdata = test,type="class")
test$prediction<- predict(mytree,newdata= x,type="class")
table(test$Species)
table(iris$Species)
table(test$predictedClass,test$Species)
sum(test$predictedClass==test$Species)/nrow(test)
