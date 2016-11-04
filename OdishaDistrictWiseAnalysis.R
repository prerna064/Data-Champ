
################Household Category##############
library(readxl)
odisha<-read_excel("Odisha.xlsx")
dim(odisha)
Households<-odisha[,c(2,3,4,5,6,7)]
library(reshape)
odishan<-reshape(temp,varying= list(c(4,5,6)),v.names="Values",timevar = "Type",times=c("GoodHouseholds","LivableHouseholds","DilapidatedHouseholds"),direction = "long")
summary(Households)
dim(Households)
str(Households)
temp<-temp[,-c(4,5,6)]
dim(temp)
odishan<-odishan[,-3]
t<-melt(odishan,id= c("DistrictName","Type","Values","id"))

p1<-aggregate(odisha$GoodHouseholds,by=list(odisha$DistrictName,odisha$TotalRuralUrban),FUN=sum)
colnames(p1)<-c("DistrictName","Type","GoodHouseholds")
p2<-aggregate(odisha$LivableHouseholds,by=list(odisha$TotalRuralUrban,odisha$DistrictName),FUN=sum)
colnames(p2)<-c("Type","DistrictName","LivableHouseholds")
p3<-aggregate(odisha$DilapidatedHouseholds,by=list(odisha$TotalRuralUrban,odisha$DistrictName),FUN=sum)
colnames(p3)<-c("Type","DistrictName","DilapidatedHouseholds")
p1$LivableHouseholds<-p2$LivableHouseholds
p1$DilapidatedHouseholds<-p3$DilapidatedHouseholds
str(p1)
p1$GoodHouseholds<-(p1$GoodHouseholds/100)
p1$LivableHouseholds<-(p1$LivableHouseholds/100)
p1$DilapidatedHouseholds<-(p1$DilapidatedHouseholds/100)
m<-subset(p1,p1$Type=="Total")
p1<-p1[-c(32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62),]

odisha<-unique(odisha)
str(odishan)
str(t)
library(tidyr)
t$value<-as.factor(t$value)
#t1<-aggregate(t$Type,by=list(t$),FUN=sum)
library(ggplot2)
ggplot(p1,aes(x= DistrictName,y= GoodHouseholds ,fill=Type))+geom_bar(stat = "Identity")+theme(axis.title.x=element_blank(),axis.text.x=element_text(angle=45,hjust = +1))
ggplot(p1,aes(x= DistrictName,y= LivableHouseholds ,fill=Type))+geom_bar(stat = "Identity")+theme(axis.title.x=element_blank(),axis.text.x=element_text(angle=45,hjust = +1))
ggplot(p1,aes(x= DistrictName,y= DilapidatedHouseholds ,fill=Type))+geom_bar(stat = "Identity")+theme(axis.title.x=element_blank(),axis.text.x=element_text(angle=45,hjust = +1))
int2+coord_fixed()
str(p1)
good<-p1[order(p1$GoodHouseholds),]
livable<-good[order(good$LivableHouseholds),]
dil<-livable[order(livable$DilapidatedHouseholds),]
final$GoodHouseholds<-NA
good$GoodHouseholds<-good$GoodHouseholds
good$LivableHouseholds<-livable$LivableHouse
good$DilapidatedHouseholds<-dil$DilapidatedHouseholds
data.frame(good, t(apply(-good, 1, rank, ties.method='DistrictName')))
#############################################################

#### latrine facility available/not available in disctricts #######

which(colnames(odisha)=="NoOfHouseholdsWithLatrine")
latrine<-odisha[,c(2,3,45,46)]
t<-t[-c(31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60),]
t<-aggregate(latrine[3:4],by=list(latrine$DistrictName,latrine$TotalRuralUrban),FUN=sum)
summary(t)
t1$NoOfHouseholdsWithLatrine<-t1$NoOfHouseholdsWithLatrine/t1$total
t1$NoOfHouseholdsWithoutLatrine<-t1$NoOfHouseholdsWithoutLatrine/t1$total
t1<-cbind(t[,3:4])
counts<-table(t$Group.1,t$Group.1)
barplot(counts,main="bar plot",col=c("darkblue","red"),legend=rownames(counts),beside=TRUE)
t$rural<-NA
t$urban<-NA
for(i in 1:nrow(t))
{
  
  ifelse(t$Group.2[i]=="Urban",
  t$urban[i]<-t$urban[i]+t$NoOfHouseholdsWithLatrine[i]+t$NoOfHouseholdsWithoutLatrine[i],
  t$rural[i]<-t$rural[i]+t$NoOfHouseholdsWithLatrine[i]+t$NoOfHouseholdsWithoutLatrine[i]
  )
}

#####lets plot pie chart of rural and urban on noofhouseholdswithlatrine
xUrban <- latrine$`TotalRuralUrban`=="Urban"
xUrbanValues <- ifelse(xUrban==TRUE,latrine$NoOfHouseholdsWithLatrine,NA)
xUrbanValues = na.omit(xUrbanValues)
m2<-sum(xUrbanValues)
xRural <- latrine$`TotalRuralUrban`=="Rural"
xRuralValues <- ifelse(xRural==TRUE,latrine$NoOfHouseholdsWithLatrine,NA)
xRuralValues = na.omit(xRuralValues)
m1<-sum(xRuralValues)


temp<-
x1<-c(m1,m2)
labels1<-c("Rural 41.6%","Urban 58.3%")
pie(x=x1,labels=labels1,col= rainbow(length(x)))
library(plotrix)
pie3D(x=x1,labels=labels1,explode=0.1,main="Number of households with latrine facility")
png(file="withlatrine.jpg")
### lets find out values for without latrine households
xrural<-latrine$TotalRuralUrban=="Rural"
xruralvalues<-ifelse(xrural==TRUE,latrine$NoOfHouseholdsWithoutLatrine,NA)
xruralvalues<-na.omit(xruralvalues)
m3<-sum(xruralvalues)

xurban<-latrine$TotalRuralUrban=="Urban"
xurbanvalues<-ifelse(xurban==TRUE,latrine$NoOfHouseholdsWithoutLatrine,NA)
xurbanvalues<-na.omit(xurbanvalues)
m4<-sum(xurbanvalues)
x<-c(m3,m4)
labels<-c("Rural 75.77%","Urban 24.22%")
pie(x,labels,col= rainbow(length(x)))
library(plotrix)
pie3D(x=x,labels=labels,explode=0.1,main="Number of households without latrine facility")
###################################################
##########################
which(colnames(odisha)=="Total number of households availing banking services")
bank<-odisha[,c(2,3,62)]
bank<-bank[-c(1,2,3),]
b<-aggregate(bank[3],by=list(bank$DistrictName,bank$TotalRuralUrban),FUN=sum)
b<-b[-c(31:60),]
ggplot(b,aes(x= Group.1,y=BankingServices ,fill=Group.2))+geom_bar(stat = "Identity")+theme(axis.title.x=element_blank(),axis.text.x=element_text(angle=45,hjust = +1))
bankmatrix<-b$BankingServices
library(plotrix)
banklabels<-b$Group.1
pie3D(x=bankmatrix,labels = banklabels,explode = 0.1,shade=0.8,col= rainbow(length(b$Group.1)),labelcex=0.5,labelrad=1.00)
#### The pie3d is not clear, lets try to find sum district wise(both rural and urban)
#for(i in 1:length(b))
#{
#urbann<-ifelse(b$Group.2[i]=="Urban",which(b$Group.1[i]),NA)
#urbann<-na.omit(urbann)
#value<-b[urbann]$BankingServices[i]
#sum(value)
#}
btemp<-subset(b,Group.2=="Urban")
btemp2<-subset(b,Group.2=="Rural")
btemp<-cbind(btemp,btemp2)
btemp$total<-c(1453.1,613.2,1034.4,599.6,296.8,569.1,1404.8,212.5,584.3,838.1,2167.9,580.9,808.9,902.8,755.4,638.5,1176.1,718.1,1689.7,960.2,1068.1,619.7,1845.9,553.6,904.9,852.8,1002.6,1080.8,478.5,1968.8)
library(ggplot2)
ggplot(btemp,aes(x= Group.1,y= total))+geom_bar(col="green",stat = "Identity")+theme(axis.title.x=element_blank(),axis.text.x=element_text(angle=45,hjust = +1))
btemp<-btemp[order(-btemp$total),]
#transform(btemp, rank = ave(total,Group.1,FUN = function(total) rank(-total, ties.method = "first")))
btemp$rank <- NA
btemp$rank[btemp$total] <- 1:nrow(btemp)
dat
data.frame(btemp, (apply(-btemp, 1, rank, ties.method='min')))

library(plyr)
ddply(btemp, .(Group.1,total), transform, rank = rank(total))
btemp<-btemp[,-4]


#################################################################################
############availability of assets ###################################################

assets<-odisha[,c(2,3,63,64,65,66,67,68,69,70,71)]
assets<-assets[-c(1:3),]
a1<-aggregate(assets$Telivision,by=list(assets$DistrictName,assets$TotalRuralUrban),FUN=sum)
colnames(a1)[3]<-"Telivision"
a2<-aggregate(assets$ComputerwithInternet,by=list(assets$DistrictName,assets$TotalRuralUrban),FUN=sum)
colnames(a2)[3]<-"ComputerwithInternet"
a3<-aggregate(assets$ComputerwithoutInternet,by=list(assets$DistrictName,assets$TotalRuralUrban),FUN=sum)
colnames(a3)[3]<-"ComputerwithoutInternet"
a4<-aggregate(assets$Landline,by=list(assets$DistrictName,assets$TotalRuralUrban),FUN=sum)
colnames(a4)[3]<-"Landline"
a5<-aggregate(assets$Mobile,by=list(assets$DistrictName,assets$TotalRuralUrban),FUN=sum)
colnames(a5)[3]<-"Mobile"
a6<-aggregate(assets$Both,by=list(assets$DistrictName,assets$TotalRuralUrban),FUN=sum)
colnames(a6)[3]<-"Both"
#a1<-aggregate(assets$Telivision,by=list(assets$DistrictName,assets$TotalRuralUrban),FUN=sum)
#a1<-aggregate(assets$Telivision,by=list(assets$DistrictName,assets$TotalRuralUrban),FUN=sum)
a1<-cbind(a1,a2,a3,a4,a5,a6)
dim(a1)
a1<-a1[,-c(4,5,7,8,10,11,13,14,16,17)]
a1<-a1[-c(31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60),]
##########################################################################
################Telivision Part#######################
District <- c()
SumValue <- c()
for (name in D_name ) {
  x <- assets$DistrictName==name
  xVal <- ifelse(x==TRUE,assets$Telivision,NA)
  xVal <- na.omit(xVal)
  sumxVal <- sum(xVal)
  print(name)
  print(sumxVal)
  
  District <- c(District,name)
  SumValue <- c(SumValue,sumxVal)
  
}

print(District)
print(SumValue)
newDataframe <- c(District,SumValue)
print(newDataframe)

temp <- list(District,SumValue)
newDataframe <- as.data.frame(temp)

colnames(newDataframe) <- c("District_name","Telivision")

newDataframe$crank <- rank(-newDataframe$Telivision)
newDataframe$crank<-sort(newDataframe$crank)
print(newDataframe)
newDataframe<-unique(newDataframe)
write.table(newDataframe, "TelivisionRanking.csv", col.names=TRUE, sep=",")
newDataframe<-newDataframe[,-3]
###############################################################
################################Mobile ##################

District<-c()
cvalue<-c()
for (name in assets$DistrictName ) {
  c <- assets$DistrictName==name
  cVal <- ifelse(c==TRUE,assets$Mobile,NA)
  cVal <- na.omit(cVal)
  sumcVal <- sum(cVal)
  print(name)
  print(sumcVal)
  
  District <- c(District,name)
  cvalue <- c(cvalue,sumcVal)
  
}

print(District)
print(cvalue)
mobileDataframe <- c(District,cvalue)
print(mobileDataframe)

mobiletemp <- list(District,cvalue)
mobileDataframe <- as.data.frame(mobiletemp)
colnames(mobileDataframe)<-c("DistrictName","Mobile")
mobileDataframe<-unique(mobileDataframe)
mobileDataframe$crank<-rank(mobileDataframe$Mobile)
mobileDataframe$crank<-sort(mobileDataframe$crank)
write.table(mobileDataframe, "MobileAssetRanking", col.names=TRUE, sep=",")
#################################################################
###computer with internet and computer without internet part ###########

District<-c()
c1value<-c()
for (name in assets$DistrictName ) {
  c1 <- assets$DistrictName==name
  c1Val <- ifelse(c1==TRUE,assets$ComputerwithInternet,NA)
  c1Val <- na.omit(c1Val)
  sumc1Val <- sum(c1Val)
  print(name)
  print(sumc1Val)
  
  District <- c(District,name)
  c1value <- c(c1value,sumc1Val)
  
}

print(District)
print(c1value)
computerDataframe <- c(District,c1value)
print(computerDataframe)

temp <- list(District,c1value)
computerDataframe <- as.data.frame(temp)
colnames(computerDataframe)<-c("DistrictName","Computer With Internet Facility")
computerDataframe<-unique(computerDataframe)
computerDataframe$crank<-rank(computerDataframe$`Computer With Internet Facility`)
computerDataframe$crank<-sort(computerDataframe$crank)
write.table(computerDataframe, "ComputerWithInternet.csv", col.names=TRUE, sep=",")
#################
District<-c()
c2value<-c()
for (name in assets$DistrictName ) {
  c2 <- assets$DistrictName==name
  c2Val <- ifelse(c2==TRUE,assets$ComputerwithoutInternet,NA)
  c2Val <- na.omit(c2Val)
  sumc2Val <- sum(c2Val)
  print(name)
  print(sumc2Val)
  
  District <- c(District,name)
  c2value <- c(c2value,sumc2Val)
  
}

print(District)
print(c2value)
computerwithoutDataframe <- c(District,c2value)
print(computerwithoutDataframe)

temp <- list(District,c2value)
computerwithoutDataframe <- as.data.frame(temp)
colnames(computerwithoutDataframe)<-c("DistrictName","Computer Without Internet Facility")
computerwithoutDataframe<-unique(computerwithoutDataframe)
write.table(computerwithoutDataframe, "ComputerWithoutInternet.csv", col.names=TRUE, sep=",")








