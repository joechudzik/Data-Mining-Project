library(igraph)
library(readtext)
library(tidyverse)
library(tnet)
library(mallet)
library(NLP)
library(tm)
library(SnowballC)
library(topicmodels)
library(tidytext)

emails <- read_csv('emails.csv')
stopwords.df <- read.table('stopwords_en.txt', stringsAsFactors = FALSE)

emails <- na.omit(emails)
remove <- which(nchar(emails$body) == 0)
if (0 != length(remove)) {
  emails <- emails[-remove,]
}

#emailbodies <- emails$body
#docs <- Corpus(VectorSource(emailbodies)) # create corpus from vector of email bodies
names(emails) = c("doc_id", "subject", "text")
emails$doc_id = as.character(emails$doc_id)
emails <- as.data.frame(emails)
docs <- Corpus(DataframeSource(emails))
#writeLines(as.character(docs[[1]]))       # inspect particular document in corpus

# removing potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, ' ', x))})
docs <- tm_map(docs, toSpace, '-')
docs <- tm_map(docs, toSpace, '\'')
docs <- tm_map(docs, toSpace, "\"")
docs <- tm_map(docs, toSpace, '\\.')
docs <- tm_map(docs, toSpace, ':')
docs <- tm_map(docs, toSpace, '@')
docs <- tm_map(docs, toSpace, '/')
docs <- tm_map(docs, toSpace, '”')
docs <- tm_map(docs, toSpace, '“')
docs <- tm_map(docs, toSpace, '‘')
docs <- tm_map(docs, toSpace, '’')

# remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
# to lower case
docs <- tm_map(docs, tolower)
#remove stopwords
#docs <- tm_map(docs, removeWords, stopwords('english'))
docs <- tm_map(docs, removeWords, stopwords.df[[1]])
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Good practice to check every now and then
#writeLines(as.character(docs[[30]]))
#Stem document
docs <- tm_map(docs, stemDocument, 'english')


################################################################################

myDf <- data.frame(text = sapply(docs, paste, collapse = " "), stringsAsFactors = FALSE)
myDf <- cbind(emails$index, myDf)
colnames(myDf) <- c("doc_id", "text")
myDf$doc_id <- as.character(myDf$doc_id)
remove <- which(nchar(myDf$text) == 0)
myDf <- myDf[-remove,]
remove <- which(myDf$text == 'NA')
myDf <- myDf[-remove,]

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

##################################################################################
# GUHA
topic.dtm <- DocumentTermMatrix(docs)
#rowSum <- apply(topic.dtm , 1, sum)
#topic.dtm <- topic.dtm[rowSum> 0, ]
ui <- unique(topic.dtm$i)
topic.dtm.ui <- topic.dtm[ui, ]
ap_lda <- LDA(topic.dtm.ui, k = 4, control = list(seed = 1234))
#
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
#
email.gamma <- tidy(ap_lda, matrix="gamma")
gamma.df <- as.data.frame(email.gamma)
write.csv(gamma.df, 'overall_gamma.csv')
email_classifications <- email.gamma %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup()


