library(igraph)
library(readtext)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms


bipartiteEdgeList <- read.csv('C:/Users/matth/Desktop/Python/BipartiteEdgeList.csv', header = TRUE)
allElements <- unique(unlist(bipartiteEdgeList))          #The set of all unique elements in the edge list
allEmails <- grep("*.eml", allElements, value= TRUE)      #The subset of unique elements that are emails
allEmailAddresses <- setdiff(allElements, allEmails)      #The subset of unique elements that are emailAddresses

#Create graph using edgeList as template
bipartiteGraph <- graph.data.frame(bipartiteEdgeList, directed = TRUE)
#Distinguish emails from addresses - 
V(bipartiteGraph)$type <- V(bipartiteGraph)$name %in% allEmails


V(bipartiteGraph)$color <- ifelse(V(bipartiteGraph)$type, "lightblue", "salmon")
V(bipartiteGraph)$size <- 4
V(bipartiteGraph)$label.cex <- .01
E(bipartiteGraph)$arrow.size <-.1


bipartiteGraph.degree <- degree(bipartiteGraph)
V(bipartiteGraph)$degree <- degree(bipartiteGraph)

bipartiteLayout <- layout_as_bipartite(bipartiteGraph)

#Writing plot to file - plot isn't terribly useful right now.
bmp(filename = 'BipartiteGraph.bmp', width = 6, height = 6, units = 'in', res = 100)
plot(bipartiteGraph, layout = bipartiteLayout)
dev.off()



#Generating a degree distribution plot of emails
onlyEmails <- delete_vertices(bipartiteGraph, V(bipartiteGraph)$type != TRUE)
onlyEmailsHistogram <- as.data.frame(table(V(onlyEmails)$degree))
onlyEmailsHistogram[,1] <- as.numeric(onlyEmailsHistogram[,1])

bmp(filename = 'EmailDegreeDistribution.bmp', width = 6, height = 6, units = 'in', res = 100)
plot(onlyEmailsHistogram, 
     log="xy", 
     xlab = "log(Email addresses sending/receiving this email)", 
     ylab = "log(Emails adjacent to this many addresses)")
dev.off()



#Generating a degree distribution plot of email addresses
onlyRecipients <- delete_vertices(bipartiteGraph, V(bipartiteGraph)$type == TRUE)
onlyRecipientsHistogram <- as.data.frame(table(V(onlyRecipients)$degree))
onlyRecipientsHistogram[,1] <- as.numeric(onlyRecipientsHistogram[,1])

bmp(filename = 'EmailAddressDegreeDistribution.bmp', width = 6, height = 6, units = 'in', res = 100)
plot(onlyRecipientsHistogram, 
     log = "xy",
     xlab = "log(Number of Emails Received)", 
     ylab = "log(Frequency of email addresses)")
dev.off()



#Converting bimodal graph to email graph
unorderedBipartiteEdges = read.csv('C:/Users/matth/Desktop/Python/UnorderedBipartiteEdgeList.csv', header = TRUE)
unorderedBipartiteGraph = graph.data.frame(unorderedBipartiteEdges)

allElements <- unique(unlist(bipartiteEdgeList))          #The set of all unique elements in the edge list
allEmails <- grep("*.eml", allElements, value= TRUE)      #The subset of unique elements that are emails
allEmailAddresses <- setdiff(allElements, allEmails)      #The subset of unique elements that are emailAddresses

V(unorderedBipartiteGraph)$type <- V(unorderedBipartiteGraph)$name %in% allEmails

bipartite.mapping(unorderedBipartiteGraph)

bipartiteMatrix <- as_incidence_matrix(unorderedBipartiteGraph)
bipartiteMatrix