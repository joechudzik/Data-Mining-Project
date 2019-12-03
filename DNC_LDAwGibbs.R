rm(list=ls())

library(igraph)
library(readtext)
library(tidyverse)
library(tnet)
#library(mallet)
library(NLP)
library(tm)
library(SnowballC)
library(topicmodels)
library(tidyr)
library(tidytext)
library(dplyr)
library(broom)

data <- read.csv('/Users/Joey/Documents/GitHub/Data-Mining-Project/data/dnc-email-out.csv')


#emails <- read.delim('/Users/Joey/Desktop/email/emails.csv', sep='\t', header=F)
#names(emails) <- c('to', 'from', 'subject', 'date')
#emails <- emails[-c(1,2,3,4,5),]

emails <- read.csv('/Users/Joey/Desktop/DataMiningProj/emails.csv')
#halfEmails <- emails[sample(1:nrow(emails), 11228, replace=FALSE),]  # get a random sample of half of the normal data
#emailbodies <- halfEmails$body
emailbodies <- emails$body
docs <- Corpus(VectorSource(emailbodies)) # create corpus from vector of email bodies
writeLines(as.character(docs[[2]]))       # inspect particular document in corpus

# removing potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, ' ', x))})
docs <- tm_map(docs, toSpace, '-')
docs <- tm_map(docs, toSpace, '\'')
docs <- tm_map(docs, toSpace, "\"")

# remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords('english'))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Good practice to check every now and then
writeLines(as.character(docs[[30]]))
#Stem document
docs <- tm_map(docs,stemDocument)


#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord]
write.csv(freq[ord],'/Users/Joey/Desktop/word_freq.csv')


# R threw some hiccups. fixing error from above. (dont know how to explain it but error is reproducable without running next 2 lines)
raw.sum <- apply(dtm, 1, FUN=sum) # sum each row of the table
dtm <- dtm[raw.sum!=0,] # delete all raws with 0

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm, k=5, control = list(seed=1234), method='Gibbs')
emails_lda <- ldaOut


emails_topics <- tidy(emails_lda, matrix = "beta")
emails_topics

# gives us the top n words for each topic
#   normally show 20 words per topic; change to ... top_n(20, beta) ...
# 
emails_top_terms <- emails_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# take the top terms and make a pretty visualization of the distribution above
#   he wants to see visualizations like this in the project
#   normally show 20 words per topic; gives a better qual observation of whats happening
emails_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# side-by-side comparison of the probability of each word in a topic
# 
beta_spread <- emails_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

# gives the probability of a topic within a document
#
emails_documents <- tidy(emails_lda, matrix = "gamma")
emails_documents

# the specific words and their frequencies within a document
#
tidy(halfEmails) %>%
  filter(document == 6) %>%
  arrange(desc(count))



# linestop
# this is stuff for the graph




g <- graph.data.frame(data, directed=FALSE)

# change the edges - add color to edges
E(g)$color <- "lightgray"
V(g)$label.color <- "black"
V(g)$label.cex <- 0.05
V(g)$from.color <- "gray"
V(g)$size <- 2
plot.igraph(g, layout=layout_with_fr)

# theres some kind of hierarchy
#plot(g, layout=layout_as_tree)

#deg <- degree(g)
#bet <- betweenness(g) # higher means more likely to be an information broker
#clos <- closeness(g)
#eig <- eigen_centrality(g)