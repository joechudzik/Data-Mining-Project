library(igraph)
library(tidyverse)
library(network)
library(sna)
library(GGally)
library(tnet)


#data.df <- read.table('email-dnc-corecipient.edges', skip = 1)
data.df <- read.csv('network.csv')

data.graph <- graph.data.frame(data.df[,c(1:3)])
data.net <- network(data.df[,c(1:3)])
data.tnet <- as.tnet(data.df[,c(1:3)], type = "weighted one-mode tnet")
data.tnet.undirect <- symmetrise_w(data.tnet)
data.degree = degree_w(data.tnet)
data.degree = as.data.frame(data.degree)

# edge size
E(data.graph)$weight <- data.df$weight
E(data.graph)$width <- ifelse(E(data.graph)$weight>142, 10, 
              ifelse(E(data.graph)$weight>99, 5, 
              ifelse(E(data.graph)$weight>61, 2, 1)))
E(data.graph)$color <- ifelse(E(data.graph)$weight>61, "blue", "lightgray")
#
V(data.graph)$degree <- data.degree[data.degree$node==V(data.graph)$name, 'degree']
for (i in (1: length(V(data.graph)$name)))
{
  V(data.graph)$degree[i] = data.degree[data.degree$node == V(data.graph)$name[i], 'degree']
}
# node color
V(data.graph)$color <- ifelse(V(data.graph)$degree>100, "red", "black")
V(data.graph)$frame.color <- ifelse(V(data.graph)$degree>100, "red", "black")
# node size
V(data.graph)$size <- ifelse(V(data.graph)$degree>100, 5, 1)
# node label
V(data.graph)$label <- ifelse(V(data.graph)$degree>100, V(data.graph)$name, "")
V(data.graph)$label <- ""

# cluster
graph.clusters <- clusters(data.graph)
cluster.vector <- graph.clusters$membership[which(graph.clusters$membership==1)]
cluster.vector <- names(cluster.vector)
graph.sub <- subgraph(data.graph, cluster.vector)
com <- cluster_spinglass(graph.sub, spins=12)

# set cluster color
for (i in 1:length(V(data.graph)$name))
{
  idx <- which(com$name==V(data.graph)$name[i])
  if (length(idx) == 0) {
    V(data.graph)$color[i] = 1
  }
  else {
    V(data.graph)$color[i] = com$membership[idx] + 1
  }
}
V(data.graph)$frame.color <- V(data.graph)$color

minC <- rep(-Inf, vcount(data.graph))
maxC <- rep(Inf, vcount(data.graph))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(data.graph, minx=minC, maxx=maxC,
                     miny=minC, maxy=maxC)
svg(filename = "plot-force-directed.svg", width = 1000, height = 1000)
plot(data.graph, layout=layout_with_fr, edge.arrow.size=0,
     rescale=FALSE, 
     xlim=range(co[,1]), ylim=range(co[,2]), 
     vertex.label.dist=0, vertex.label.color="green", vertext.label.cex=50000)
dev.off()
svg(filename = "plot-with-clusters.svg")
plot(data.graph, layout=layout_with_fr, edge.arrow.size=0,
     rescale=FALSE, 
     xlim=range(co[,1]), ylim=range(co[,2]))
dev.off()
