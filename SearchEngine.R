##########Search Engine ##############
#########Pretend this is big data:########
doc1 <- "Stray cats are running all over the place. I see 10 a day!"
doc2 <- "Cats are killers. They kill billions of animals a year."
doc3 <- "The best food in Columbus, OH is   the North Market."
doc4 <- "Brand A is the best tasting cat food around. Your cat will love it."
doc5 <- "Buy Brand C cat food for your cat. Brand C makes healthy and happy cats."
doc6 <- "The Arnold Classic came to town this weekend. It reminds us to be healthy."
doc7 <- "I have nothing to say. In summary, I have told you nothing."

#########and this is the big file system ####
doc.list<- list(doc1,doc2,doc3,doc4,doc5,doc6,doc7)
N.docs<-length(doc.list)
names(doc.list)
names(doc.list)<-paste0("doc",c(1:N.docs))
query <- "Healthy cat food"
########lets start the text mining process#######
#######load tm package #######

###step 1#######
library(tm)
##### step 2 make a corpus#######
my.docs<-VectorSource(c(doc.list,query))
my.docs$Names<-c(names(doc.list),"query")
my.docs$Names
my.corpus<-Corpus(my.docs)


######## step2 cleaning and transforming the data############
#my.corpus<-content_transformer(my.corpus)
my.corpus
my.corpus<-tm_map(my.corpus,content_transformer(tolower))
my.corpus<-tm_map(my.corpus,removeWords,stopwords("english"))
my.corpus<-tm_map(my.corpus,removePunctuation)
my.corpus<-tm_map(my.corpus,removeNumbers)
my.corpus<-tm_map(my.corpus,stripWhitespace)
my.corpus<- tm_map(my.corpus,stemDocument)

#########step 3 creating document term matrix
dtm<-DocumentTermMatrix(my.corpus)
inspect(dtm)
#######step 4 calculate tfidf
result<-weightTfIdf(dtm)
inspect(result)

###########step 5 calculate pagerank
m<-matrix(result)
m<-m[,m[8,]>0]
#lets calculate cosine coefficient#
cosine<-0*(1:N.docs)
cosine
for(i in 1:N.docs)
{
  cosine[i]<- sum(m[i,]*m[8,])/sum(m[i,]^2)^0.5*sum(m[8,]^2)^0.5
}
EuclCoeff<-0*(1:N.docs)
for (i in 1:N.docs) {
  EuclCoeff[i]<-sum((m[i,]-m[8,])^2)^0.5
}
pagerank<-order(EuclCoeff)
rankedpages<-doc.list[order(EuclCoeff)]
rankedpages
