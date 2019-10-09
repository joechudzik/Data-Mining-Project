rm(list=ls())

#install.packages("ggplot2")

library(igraph)
library(readtext)
library(tidyverse)
library(ggplot2)

data <- read.csv('C:/Users/Matthew/Desktop/Data-Mining-Project/data/dnc-email-out-headers.csv')

g <- graph.data.frame(data)
g <- graph.data.frame(data, directed=TRUE)

V(g)$label.color <- "black"

V(g)$from.color <- "gray"
V(g)$label.cex <- 1


# v(g)
# V(g)$type <- bipartite_mapping(g)$type
# bipartite.mapping(g) # tells us if plottable graph

# plot(g)

# plot(g, vertex.label.cex=0.8, vertex.label.color="black")

# making groups and nodes look visually different (this one is color)
# V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")

# making the groups and nodes look visually different (this one is shape)
# V(g)$shape <- ifelse(V(g)$type, "circle","square")

# change the edges - add color to edges
# E(g)$color <- "lightgray"


# v(g)$from.size <- 1
V(g)$size <- 4
# plot(g, layout=layout_with_graphopt)
v(g)$from.size <- 1
# plotting bimodal networks - gives more traditional bipartite look
# will be useful for the analysis

types <- V(g)$type
deg <- degree(g)
bet <- betweenness(g)
clos <- closeness(g)
eig <- eigen_centrality(g)

degreeDistribution <- degree_distribution(g, cumulative = TRUE)

plot(g, sertex.size=2, vertex.label.cex=0.6)
plot(degreeDistribution)
plot(deg)



# List of degrees
G.degrees <- degree(g)

# Let's count the frequencies of each degree
G.degree.histogram <- as.data.frame(table(G.degrees))

# Need to convert the first column to numbers, otherwise
# the log-log thing will not work (that's fair...)
G.degree.histogram[,1] <- as.numeric(G.degree.histogram[,1])

# Now, plot it!
# ggplot(G.degree.histogram, aes(x = G.degrees, y = Freq)) +
#  geom_point() +
#  scale_x_continuous("Degree\n(nodes with this amount of connections)",
#                     breaks = c(1, 3, 10, 30, 100, 300),
#                     trans = "log10") +
#  scale_y_continuous("Frequency\n(how many of them)",
#                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
#                     trans = "log10") +
#  ggtitle("Degree Distribution (log-log)") +
#  theme_bw()

# Now, plot it!
ggplot(G.degree.histogram, aes(x = G.degrees, y = Freq)) +
  geom_point() +
  scale_x_continuous("Degree\n(nodes with this amount of connections)",
                     breaks = c(1, 3, 10, 30, 100, 300),
                     ) +
  scale_y_continuous("Frequency\n(how many of them)",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
                     ) +
  ggtitle("Degree Distribution (log-log)") +
  theme_bw()
