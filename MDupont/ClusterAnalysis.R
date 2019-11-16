library(igraph)
library(readtext)
library(tidyverse)
#library(ggplot2)
library(reshape2)
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

standardEdgeList <- read.csv('C:/Users/matth/Desktop/Python/EmailNetworkEdgeList.csv')
emailRecipientGraph <- graph.data.frame(standardEdgeList)
undirectedEmailRecipientGraph <- graph.data.frame(standardEdgeList, directed = FALSE)
undirectedEmailRecipientGraph <- centroid(undirectedEmailRecipientGraph)

V(emailRecipientGraph)$size <- 1
V(emailRecipientGraph)$label.cex <- .01
E(emailRecipientGraph)$arrow.size <-.1
V(emailRecipientGraph)$vertex_degree <- degree(emailRecipientGraph)
V(emailRecipientGraph)$degree <- degree(emailRecipientGraph)
emailRecipientLayout <- layout_with_fr(emailRecipientGraph, grid='nogrid')

png('Force-Directed-EmailGraph.png')
plot.igraph(emailRecipientGraph,
     layout = emailRecipientLayout,
     vertex.size = (V(emailRecipientGraph)$degree*8) ^ (1/8)#,
     #edge.width = E(emailRecipientGraph)$weight * (1/100)
     )
dev.off()


#email_walktrap_clusters = walktrap.community(emailRecipientGraph)
#email_walktrap_clusters
#email_walktrap_clusters_dendrogram = as.dendrogram(email_walktrap_clusters, modularity=TRUE)
#plot(email_walktrap_clusters_dendrogram)

#community <- cluster_edge_betweenness(emailRecipientGraph)
#community
simplify(undirectedEmailRecipientGraph)

clusters <- cluster_louvain(undirectedEmailRecipientGraph)
modularity(clusters)
V(emailRecipientGraph)$membership <- membership(clusters)
V(emailRecipientGraph)$color <- V(emailRecipientGraph)$membership

G_Grouped <- emailRecipientGraph
E(G_Grouped)$weight = 1
for(i in unique(V(emailRecipientGraph)$membership)){
  MemberGroup <- which(V(emailRecipientGraph)$membership == i)
  if(length(MemberGroup) > 1) G_Grouped = add_edges(G_Grouped, combn(MemberGroup, 2), attr=list(weight=5))
}
newFR <- layout_with_fr(G_Grouped, grid='nogrid')


V(undirectedEmailRecipientGraph)$size <- 2
V(undirectedEmailRecipientGraph)$label.cex <- .01
E(undirectedEmailRecipientGraph)$arrow.size <-.1
V(undirectedEmailRecipientGraph)$vertex_degree <- degree(emailRecipientGraph)
V(undirectedEmailRecipientGraph)$degree <- degree(emailRecipientGraph)

plot(undirectedEmailRecipientGraph, layout = newFR)
