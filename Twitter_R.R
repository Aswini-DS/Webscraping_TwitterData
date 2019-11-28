#install.packages("twitteR")
#install.packages("ROAuth")
#install.packages("base64enc")
#install.packages("httpuv")

library("twitteR")
library("ROAuth")

cred <- OAuthFactory$new(consumerKey='RMDaQooCpaIn3DgYGZaAcp9Px', # Consumer Key (API Key)
                         consumerSecret='p4SWeHcFeT61nK230es7MXPsLWSVbAALGw7bWVfvXJvIcJVVgP', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

library(base64enc)

library(httpuv)

setup_twitter_oauth("RMDaQooCpaIn3DgYGZaAcp9Px", # Consumer Key (API Key)
                    "p4SWeHcFeT61nK230es7MXPsLWSVbAALGw7bWVfvXJvIcJVVgP", #Consumer Secret (API Secret)
                    "2197727629-7jDmRrGOphOfpXGKDiRA2LXZcnWfqvU5neXMyx7",  # Access Token
                    "E1ZYLWd69fCZg2dhyPU1wtQhe7BVDLzrMCZIPUipD1VdM")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('DataScience', n = 200,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "Tweets_DataScience.txt",row.names = F)


library("rJava")
library("tm")
library("SnowballC")
library("wordcloud")
library("RWeka")
library("textir")
library("igraph")
library("qdap")
library("maptpx")
library("data.table")
library("stringr")
library("slam")
library("ggplot2")

usertweets <- readLines("C:\\Users\\Lucky\\Downloads\\DataSets\\Tweets_DataScience.txt")
usertweets[1:5]
usertweets <- stemDocument(usertweets,language = "english")

corpusdata <- Corpus(VectorSource(usertweets))
inspect(corpusdata)[1:2]

corpusdata <- tm_map(corpusdata, content_transformer(tolower))
corpusdata <- tm_map(corpusdata, removeNumbers)
corpusdata <- tm_map(corpusdata, removeWords, stopwords("english"))
corpusdata <- tm_map(corpusdata, removeWords, c("science", "data")) 
corpusdata <- tm_map(corpusdata, removePunctuation)
corpusdata <- tm_map(corpusdata, stripWhitespace)

# Text stemming
corpusdata <- tm_map(corpusdata, stemDocument)

tdm0 = TermDocumentMatrix(corpusdata)
class(tdm0)
inspect(tdm0)[1:3]

tmd1 <- TermDocumentMatrix(corpusdata,control = list(weighting = function(p) weightTfIdf(p,normalize = T)))

a0 <- NULL
a1 <- NULL

for(i in 1:ncol(tdm0))
{
  if(sum(tdm0[,i])==0) {a0=c(a0,i)}
  for(i in 1:ncol(tmd1)){
    if(sum(tmd1[,i])==0) {a1=c(a1,i)}
  }    
}

tdm0 <- tdm0[,-a0]
tmd1 <- tmd1[,-a1]

dtm0 <- t(tdm0)
dtm1 <- t(tmd1)

dtm1.colsum = apply(dtm1, 2, sum);
words <- colnames(dtm1)
freq <- dtm1.colsum*100

wordcloud(words,freq = freq, scale=c(8, 0.3), colors=1:10)
title(sub = "UNIGRAM - Wordcloud using TFIDF")

pos.words <- scan(file.choose(),what = "character",comment.char = ";")
neg.words <- scan(file.choose(),what = "character",comment.char = ";")

#Positive word cloud
pos.matches = match(colnames(dtm1), pos.words)
pos.matches = !is.na(pos.matches)
b1 = apply(dtm1, 2, sum)[pos.matches];    b1 = as.data.frame(b1);
colnames(b1) = c("freq");

wordcloud(rownames(b1), b1[,1], scale=c(5, 1), colors=1:10)

#Negative word cloud

neg.matches = match(colnames(dtm1), neg.words)       # match() returns the position of the matched term or NA
neg.matches = !is.na(neg.matches)
b1 = apply(dtm1, 2, sum)[neg.matches];    b1 = as.data.frame(b1);
colnames(b1) = c("freq");
wordcloud(rownames(b1), b1[,1], scale=c(5, 1), colors=1:10)
