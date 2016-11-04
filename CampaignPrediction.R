library(ggplot2)
library(plyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(tree)
library(caTools)
train<-read.csv("Campaign.csv",header = T)
summary(train)
## so we dont have any NA values, minimum age is 17 and maximum age is 98, as I can see response for "no" is far more than response for "yes"


####lets understand our data piece by piece########
ggplot(train,aes(x= response,fill= marital))+geom_bar()+coord_polar(theta="y")+scale_x_discrete()
ggplot(train)+geom_bar(aes(job))+coord_flip()
ggplot(train)+geom_bar(aes(education))+coord_flip()
ggplot(train,aes(day_of_week,duration))+geom_point()
#### here, we have got an interesting insight that on monday we have got one value whose duration is approx 4800
ggplot(train,aes(month,duration))+geom_point()
#### again we found that value for the month of number so lets find out wats this value
out<-subset(train,(duration>4800 & month=="nov"))
out
## so there is a person who had duration of 4918  over telephone on monday in the month of november took housing loan with no default.But has no response and poutcome is nonexistent.

##### frequency table for categorical variables####
as.character(train$job)
job.count<-data.frame(table(train$job))
job.count
education.count<-data.frame(table(train$education))
education.count
housing.count<- data.frame(table(train$housing))
housing.count
default.count<- data.frame(table(train$default))
default.count
train$default<- as.numeric(train$default)
hist(train$default,colors="blue")
#### as we found out that there were only 3 defaulters, lets see who are these defaulters
def.list
### there are 3 defaulters using cellular contact,2 is technician and 1 is unemployed.each of them is married and has response "no"
loan.count<- data.frame(table(train$loan))
loan.count
min(train$duration)
train$duration<-sort(train$duration,decreasing = TRUE)
head(train$duration,n=10)
table(train$duration,train$response)
ggplot(data=train)+geom_line(aes(train$response,train$duration),subset= .(train$response %in% c(4918,4199,3785,3643,3631,3509,3422,3366,3322,3284)))


###### lets use random forest  to find out more about the dataset#####
class(train$response)
## we checked the class of response variable, its a factor so now we can build our tree
fit<-tree(response~.,data=train,method=class)
plot(fit)
text(fit)
##### we are not getting in much detail about the things from the tree, lets use random forest for variable importance
varnames<-names(train)
varnames[!varnames %in% c("response")]
varnames1<-paste(varnames,collapse = "+")
rf.form<-as.formula(paste("response",varnames1,sep="~"))
imp.variable<-randomForest(rf.form,train,ntree=500,importance=T)
####make row names as columns
imp.variable$MeanDecreaseGini
#### variable importancetable ####
important.variable<-data.frame(importance(imp.variable,type=2))
important.variable
####### we got the list of important variables and non important variables to build our model effectively.

####### just checking duration variable ####
temp.no<- subset(train,response=="no")
temp.yes<-subset(train,response=="yes")
ggplot(temp.yes)+geom_line(aes(temp.yes$marital,train$duration))
hist(temp.yes$duration,main="duration when the response is yes",xlim = c(0,500),breaks=5)
dr<-subset(train,duration>1000)
temp.yes$response<-as.numeric(temp.yes$response)
sum(temp.yes$day_of_week== friday)
table(temp.no$day_of_week)
s
hist(temp.yes$pdays,freq = F,col ="pink")
hist(temp.no$pdays,freq = F, col= "light blue")

###### use temp.yes with important variables ########


##########################################################

##### using logistic regression algorithm to build the model and do predictive analysis and finding accuracy

model<- glm(response~.,family = "binomial",data=train)
summary(model)
pred<-predict(model,data=train,type="response")
##### confusion matrix
table(train$response,pred>0.5)
#### we got accuracy of 90.02%#########

