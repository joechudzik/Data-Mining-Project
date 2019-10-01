rm(list=ls())

library(igraph)
library(readtext)
library(tidyverse)

data <- read.csv('/Users/Joey/Desktop/dnc-email-out.csv')

g <- graph.data.frame(data)




g <- graph.data.frame(data, directed=TRUE)
V(g)$type <- bipartite_mapping(g)$type
bipartite.mapping(g) # tells us if plottable graph
plot(g)

plot(g, vertex.label.cex=0.8, vertex.label.color="black")

# making groups and nodes look visually different (this one is color)
V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")

# making the groups and nodes look visually different (this one is shape)
V(g)$shape <- ifelse(V(g)$type, "circle","square")

# change the edges - add color to edges
E(g)$color <- "lightgray"

plot(g, vertex.label.cex=0.8, vertex.label.color="black")

V(g)$label.color <- "black"

V(g)$label.cex <- 1
V(g)$from.color <- "gray"
V(g)$size <- 18
plot(g, layout=layout_with_graphopt)

# plotting bimodal networks - gives more traditional bipartite look
# will be useful for the analysis
plot(g, layout=layout.bipartite, sertex.size=7, vertex.label.cex=0.6)

types <- V(g)$type
deg <- degree(g)
bet <- betweeness(g)
clos <- closeness(g)
eig <- eigen_centrality(g)

