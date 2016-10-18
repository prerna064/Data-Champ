remove.packages(tm)
install.packages("http://cran.r-project.org/bin/windows/contrib/3.0/tm_0.5-10.zip",repos=NULL)
library(tm)


library(tm)
library(twitteR)
setwd("C:/Users/navin/Desktop/Edureka/module3/Tweets")

#Twitter Key
APIKey="I7J2zKAcFWOCN5DUrHXP7Spe6"
#Twitter Secret
APISecret="bC7SvPpdazrQWb1ipJtpkjBmBjw7KerjmWMVYA5uk6vgUhaLel"

#AccessToken
AToken="768020608487567360-DQ151QHQZTppGPn43N0wtbXYdNZf7a8"

ATokenSecret="WqVXE5C6CE9LwMSGyyK9eFBpUMi9j6TApPxUmxVWsvKGd"

setup_twitter_oauth(APIKey, APISecret, AToken,ATokenSecret)

#Get UserTweets
TrumpTweets<-rdmTweets <- userTimeline("realdonaldtrump", n=100)

#SearchTwitter
HCTweets <- searchTwitter('Clinton', n=100)



#GET TWEETING SOURCE for hillary

sources <- sapply(TrumpTweets, function(x) x$getStatusSource())
sources <- gsub("</a>", "", sources)
sources <- strsplit(sources, ">")
sources <- sapply(sources, function(x) ifelse(length(x) > 1, x[2], x[1]))
source_table = table(sources)
pie(source_table[source_table > 10])



#GET TWEETING SOURCE for trump campaign

sources <- sapply(TrumpTweets, function(x) x$getStatusSource())
sources <- gsub("</a>", "", sources)
sources <- strsplit(sources, ">")
sources <- sapply(sources, function(x) ifelse(length(x) > 1, x[2], x[1]))
source_table = table(sources)
pie(source_table[source_table > 10])



#creating a word cloud
#Convert Tweets to data frame
df <- twListToDF(TrumpTweets)
head(df)

#create a corpus
myCorpus <- Corpus(VectorSource(df$text))

#cleanup the text
 myCorpus <- tm_map(myCorpus, content_transformer(tolower))

 # remove punctuation
 myCorpus <- tm_map(myCorpus, removePunctuation)
 # remove numbers
 myCorpus <- tm_map(myCorpus, removeNumbers)
# remove stopwords
 # keep words by removing it from stopwords
# myStopwords <- c(stopwords('english'), "available", "via")
myStopwords <- stopwords('english')
 myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

## Eliminating Extra White Spaces
 myCorpus<- tm_map( myCorpus, stripWhitespace)

#remove stemdocument
dictCorpus <- myCorpus
#remove plurals
library(SnowballC)
 myCorpus <- tm_map( myCorpus, stemDocument)



#build a term document matrix
 myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
myDtm



#find frequent terms
findFreqTerms(myDtm, lowfreq=10)

#find associations
findAssocs(myDtm, "trump", 0.30)


#create a word cloud

library(wordcloud)
m <- as.matrix(myDtm)
 # calculate the frequency of words
 v <- sort(rowSums(m), decreasing=TRUE)
 myNames <- names(v)
 d <- data.frame(word=myNames, freq=v)
 wordcloud(d$word, d$freq, min.freq=3)


