#network analysis
# import text dataset
wine_reviews <- read.csv("C:/Users/Yiwen/Downloads/wine_reviews.csv")

# Build corpus
library(tm)
library(NLP)
net <- iconv(wine_reviews$Reviews.Text, to = "utf-8")
net <- Corpus(VectorSource(net))

# Clean text
# Convert all the character to lowercase
# Remove all the punctuation
# Remove all the Numeric
# Remove common english word
net <- tm_map(net, tolower)
net <- tm_map(net, removePunctuation)
net<- tm_map(net, removeNumbers)
cleandata <- tm_map(net, removeWords, stopwords('english'))

tdm <- TermDocumentMatrix(cleandata)
tdm <- as.matrix(tdm)
tdm <- tdm[rowSums(tdm)>270,]
tdm[1:10,1:10]

# Network of terms
library(igraph)
tdm[tdm>1] <- 1
termM <- tdm %*% t(tdm)
termM[1:10,1:10]
network_graph <- graph.adjacency(termM, weighted = T, mode = 'undirected')
network_graph
network_graph <- simplify(network_graph)
V(network_graph)$label <- V(network_graph)$name
V(network_graph)$degree <- degree(network_graph)

# Network diagram
set.seed(222)
plot(network_graph)

# By cluster_edge_betweenness
clus1 <- cluster_edge_betweenness(network_graph)
plot(clus1, network_graph)

# By cluster_fast_greedy
clus2 <- cluster_fast_greedy(as.undirected(network_graph))
plot(clus2, as.undirected(network_graph))

# Hub and authorities
hub <- hub_score(network_graph, weights = NA)$vector
authorities <- authority_score(network_graph, weights=NA)$vector
par(mfrow=c(1,2))
plot(network_graph, vertex.size=hub*30, main='Hubs',
     vertex.color="yellow")
plot(network_graph, vertex.size=authorities*30, main='Authorities',
     vertex.color="green")
par(mfrow=c(1,1))


# Highlighting degrees
V(network_graph)$label.cex <- 2.2*V(network_graph)$degree / max(V(network_graph)$degree) + 0.3
V(network_graph)$label.color <- rgb(0, 0, .2, .8)
V(network_graph)$frame.color <- NA
egam <- (log(E(network_graph)$weight)+.4) / max(log(E(network_graph)$weight) + .4)
E(network_graph)$color <- rgb(.5, .5, 0, egam)
E(network_graph)$width <- egam
plot(network_graph,
     vertex.color='yellow',
     vertex.size = V(network_graph)$degree*.5)

# Network of Wine_Text
textn <- t(tdm) %*% tdm
network_graph <- graph.adjacency(textn, weighted = T, mode = 'undirected')
V(network_graph)$degree <- degree(network_graph)
network_graph <- simplify(network_graph)
hist(V(network_graph)$degree,
     breaks = 100,
     col = 'blue',
     main = 'Histogram of Degree',
     ylab = 'Freuqency',
     xlab = 'Degree')






