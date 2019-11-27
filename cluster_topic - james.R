library(igraph)
library(tidyverse)
library(network)
library(sna)
library(GGally)
library(tnet)
library(mallet)
library(tidyr)
library(tm)
library(topicmodels)
library(tidytext)

stopwords.df <- read.table('stopwords_en.txt', stringsAsFactors = FALSE)
data.df <- read.table('email-dnc-corecipient.edges', skip = 1)
email.df <- read_csv('emails.csv')
email.df <- na.omit(email.df)

data.graph <- graph.data.frame(data.df)

# cluster
graph.clusters <- clusters(data.graph)
cluster.vector <- graph.clusters$membership[which(graph.clusters$membership==1)]
cluster.vector <- names(cluster.vector)
graph.sub <- subgraph(data.graph, cluster.vector)
com <- cluster_spinglass(graph.sub, spins=12)

#
emailbodies <- email.df$body
docs <- Corpus(VectorSource(emailbodies)) # create corpus from vector of email bodies
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, ' ', x))})
docs <- tm_map(docs, toSpace, '-')
#docs <- tm_map(docs, toSpace, '\'')
docs <- tm_map(docs, toSpace, "\"")
docs <- tm_map(docs, toSpace, '\\.')
docs <- tm_map(docs, toSpace, ':')
docs <- tm_map(docs, toSpace, '@')
docs <- tm_map(docs, toSpace, '/')
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
#docs <- tm_map(docs, removeWords, stopwords('english'))
docs <- tm_map(docs, removeWords, stopwords.df[[1]])
docs <- tm_map(docs, toSpace, '\'')
docs <- tm_map(docs, stripWhitespace)
#writeLines(as.character(docs[[30]]))
docs <- tm_map(docs, stemDocument, 'english')
email.topic <- data.frame(text = sapply(docs, paste, collapse = " "), stringsAsFactors = FALSE)
email.topic <- cbind(email.df$index, email.topic)
colnames(email.topic) <- c("doc_id", "text")
email.topic$doc_id <- as.character(email.topic$doc_id)
email.topic <- na.omit(email.topic)
remove <- which(nchar(email.topic$text) == 0)
email.topic <- email.topic[-remove,]

for (j in 1:length(com$csize))
{
  #
  topic.df <- NULL
  for (i in 1:length(com$names))
  {
    if (com$membership[i] != j) {
      next
    }
    topic.df <- rbind(topic.df, email.topic[email.topic$doc_id==com$names[i], ])
  }
  
  topic.dtm <- DocumentTermMatrix(Corpus(DataframeSource(topic.df)))
  rowSum <- apply(topic.dtm , 1, sum)
  topic.dtm <- topic.dtm[rowSum> 0, ]
  ap_lda <- LDA(topic.dtm, k = 2, control = list(seed = 1234))
  ap_topics <- tidy(ap_lda, matrix = "beta")
  ap_top_terms <- ap_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  ap_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered() + 
    labs(title=paste("cluster ", j))
}
