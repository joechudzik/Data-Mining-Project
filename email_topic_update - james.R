library(igraph)
library(readtext)
library(tidyverse)
library(tnet)
library(mallet)
library(NLP)
library(tm)
library(SnowballC)
library(topicmodels)

emails <- read_csv('emails.csv')

emailbodies <- emails$body
docs <- Corpus(VectorSource(emailbodies)) # create corpus from vector of email bodies
writeLines(as.character(docs[[1]]))       # inspect particular document in corpus

# removing potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, ' ', x))})
docs <- tm_map(docs, toSpace, '-')
docs <- tm_map(docs, toSpace, '\'')
docs <- tm_map(docs, toSpace, "\"")
docs <- tm_map(docs, toSpace, '\\.')

# remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
#docs <- tm_map(docs, removeWords, stopwords('english'))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
# to lower case
docs <- tm_map(docs, tolower)
#Good practice to check every now and then
writeLines(as.character(docs[[30]]))
#Stem document
docs <- tm_map(docs, stemDocument)

#myCorpusTokenized <- lapply(docs, scan_tokenizer)
#myTokensStemCompleted <- lapply(myCorpusTokenized, stemCompletion, docs)
#myDf <- data.frame(text = sapply(myTokensStemCompleted, paste, collapse = " "), stringsAsFactors = FALSE)
myDf <- data.frame(text = sapply(docs, paste, collapse = " "), stringsAsFactors = FALSE)
myDf <- cbind(emails$index, myDf)
colnames(myDf) <- c("id", "text")
myDf$id <- as.character(myDf$id)

mallet.instances <- mallet.import(myDf$id, myDf$text, "stopwords_en.txt", 
                                  FALSE, token.regexp="[\\p{L}']+")
topic.model <- MalletLDA(num.topics=100)
topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()

# examine some of the vocabulary
vocabulary[1:50]

word.freqs <- mallet.word.freqs(topic.model)
# examine some of the word frequencies:
head(word.freqs)

topic.model$setAlphaOptimization(40, 80)
topic.model$train(400)

topic.words.m <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)

# how big is the resulting matrix?
dim(topic.words.m)

# set the column names to make the matrix easier to read:
colnames(topic.words.m) <- vocabulary

# examine a specific topic
topic.num <- 1 # the topic id you wish to examine
num.top.words<-10 # the number of top words in the topic you want to examine

mallet.top.words(topic.model, topic.words.m[topic.num,], num.top.words)

# Visualize topics as word clouds
# be sure you have installed the wordcloud package
library(wordcloud)
topic.num <- 1
num.top.words<-100
topic.top.words <- mallet.top.words(topic.model, topic.words.m[1,], 100)
wordcloud(topic.top.words$words, topic.top.words$weights, c(4,.8), rot.per=0, random.order=F)

num.topics<-43
num.top.words<-25
for(i in 1:num.topics){
  topic.top.words <- mallet.top.words(topic.model, topic.words.m[i,], num.top.words)
  wordcloud(topic.top.words$words, topic.top.words$weights, c(4,.8), rot.per=0, random.order=F)
}
