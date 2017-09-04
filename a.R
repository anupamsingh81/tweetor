library(twitteR)
library(graphTweets)
library(igraph)
library(qgraph)
library(tidyverse)

library(networkD3)

str(edges)
# souce
# 
#http://rischanlab.github.io/Retweet_Graph.html
# http://kateto.net/networks-r-igraph
# https://christophergandrud.github.io/networkD3/


api_key = 'qjrxTtjGWzf2WewvRgrBmRqcC'
api_secret = 'EyHZAcmFUNwBVHMgpkfTQ18W0npSqjNPj8qU1uZ7MH4Ar5E7xb'
access_token = "56173323-ViSSDypCPtlnKgzZUqLQuVOv1al8zBkD46teok9w8"
accees_token_secret = "XRFDZzA4jpUXHaRiyDFxtA0utP1LBmKvpalFxNKKD2QiS"

setup_twitter_oauth(api_key,api_secret,access_token,accees_token_secret)

tweets <- searchTwitter("#ECNeph",since = "2017-08-27", n=300)
tweets <- twListToDF(tweets)

library(graphTweets)

edges <- getEdges(data = tweets, tweets = "text", source = "screenName")
nodes <- getNodes(edges)

# plot
g <- igraph::graph.data.frame(edges, directed=TRUE, vertices = nodes)

plot(g)



V(g)$degree <- degree(g, mode="all")
cut.off <- mean(V(g)$degree)
sub <- induced_subgraph(g, which(V(g)$degree>mean))

plot(sub,layout=layout.by.attr(sub, wc=1)) # prevents text collision


E(sub)$width <- 1+E(sub)$weight/12
plot(sub,vertex.label.distance=1.2)


cut.off

V(sub)$value
plot(sub, vertex.shape="none", vertex.size=1,     
     
     layout=layout.by.attr(sub, wc=1))

plot(sub, vertex.shape="none", vertex.size=1,     
     vertex.label.color=ifelse(V(sub)$value=="l", "blue", "red"),   
     layout=layout_with_fr)


wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}

# Apply the function to wrap the node labels
V(sub)$label = wrap_strings(V(sub)$label, 12)


## Shrink font
V(sub)$label.cex = 0.8

# Function to increase node separation (for explanatory details, see the link below)
# Source: http://stackoverflow.com/a/28722680/496488
layout.by.attr <- function(graph, wc, cluster.strength=1,layout=layout.auto) {  
  g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
  E(g)$weight <- 1
  
  attr <- cbind(id=1:vcount(g), val=wc)
  g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
  
  l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
  return(l)
}


plot(sub, vertex.shape="none", vertex.size=1,     
     vertex.label.color=ifelse(V(sub)$value=="l", "blue", "red"),
     layout=layout.by.attr(sub, wc=1))



net=g
deg <- degree(net, mode="all")
V(net)$size <- deg*3
# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight/12
plot(net) 
net <- graph.data.frame(edges, directed=T)

# simplify network
net <- simplify(net)

# adjust the size of nodes based on in and out degrees
deg <- degree(net, mode="all")
V(net)$size <- deg*0.05 + 1
V(net)[name == "arvindcanchi"]$size <- 15

plot(net, edge.arrow.size=0.1,
     vertex.label = ifelse(V(net)$size >= 15, V(net)$name, NA))

#qgraph modify

e <- get.edgelist(g)
m <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(g),
                                       area=8*(vcount(g)^2),repulse.rad=(vcount(g)^3.1))
plot(g,layout=m,vertex.size=4,vertex.label=NA)
mtext("qgraph.layout.fruchtermanreingold modified", side=1)


e <- get.edgelist(g,names=FALSE)
l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(g))
plot(g,layout=l,vertex.size=4)

l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(g),
                                       area=8*(vcount(g)^2),repulse.rad=(vcount(g)^3.1))
plot(g,layout=l,vertex.size=4,vertex.label=NA)

nodes
edges
edges$
  
  networkData <- data.frame(edges$source,edges$target)

# Plot
simpleNetwork(networkData)

?simpleNetwork
simpleNetwork(networkData, Source = 1, Target = 2,
              linkDistance = 20, charge = -30, fontSize = 5, fontFamily = "serif",
              linkColour = "#666", nodeColour = "#3182bd", opacity = 0.6, zoom = T)


hc <- hclust(dist(USArrests), "ave")

dendroNetwork(hc, height = 600)

library(igraph)
g <- graph.data.frame(edges, directed = T)
# Assign labels to the graph (=people's names)
V(g)$label <- V(g)$name


# Plot the graph using plot() or tkplot().
tkplot(g)

k= plot(g)


ig <- graph.adjacency(edges, mode="directed", weighted=TRUE) 
plot(ig)


set.seed(1492) 

l <- layout.fruchterman.reingold(g, niter=5000)

plot(g, layout=l, 
     edge.arrow.size=0.5, 
     vertex.label.cex=0.75, 
     vertex.label.family="Helvetica",
     vertex.label.font=2,
     vertex.shape="circle", 
     vertex.size=1, 
     vertex.label.color="black", 
     edge.width=0.5)

m = get.edgelist(g)
m = as.matrix(m)
lo <- qgraph.layout.fruchtermanreingold(m, repulse.rad = vcount(g)^2.8, 
                                  area = vcount(g)^2.3, niter = 1000)
plot(g, layout = lo, vertex.size = 3, vertex.frame.color = NULL, 
     vertex.label.dist = 0.5, vertex.label.cex = 0.7, edge.width = 0.5)
node.size= c(10,10,10)
plot(g, vertex.size=node.size*0.25)

str(edges)

m = as.matrix(edges)

# https://stackoverflow.com/questions/38999656/increasing-spaces-between-vertices-for-r-igraph

wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}

# Apply the function to wrap the node labels
V(g)$label = wrap_strings(V(g)$label, 12)

## Shrink font
V(g)$label.cex = 0.8

# Function to increase node separation (for explanatory details, see the link below)
# Source: http://stackoverflow.com/a/28722680/496488
layout.by.attr <- function(graph, wc, cluster.strength=1,layout=layout.auto) {  
  g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
  E(g)$weight <- 1
  
  attr <- cbind(id=1:vcount(g), val=wc)
  g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
  
  l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
  return(l)
}

## Make layout reproducible. Different values will produce different layouts,
##  but setting a seed will allow you to reproduce a layout if you like it.
set.seed(3)

plot(g, vertex.shape="none", vertex.size=1,     
     vertex.label.color=ifelse(V(g)$value=="l", "blue", "red"),
     layout=layout.by.attr(g, wc=1))

plot(g, layout=layout.by.attr(g, wc=1))

h <- graph.edgelist(get.edgelist(g)) # create a lightweight copy of graph w/o the attributes.
E(g)$weight <- 1

attr <- cbind(id=1:vcount(g), val=wc)
g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)

l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
return(l)

graph= as.matrix(edges$source,edges$target)

k = plot(graph)

h <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
E(g)$weight <- 1



is_simple(g)

plot(g, edge.arrow.size=.4,vertex.label=NA)



plot(g, layout=layout.circle, vertex.size=4,
     vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)

deg = degree(g,mode = "all")

V(g)$size<- deg*0.05 + 1

plot(g, layout=layout.auto, vertex.size=4,
     vertex.label.dist=0.9, vertex.color="red", edge.arrow.size=0.9)


plot(g, layout=layout.random, vertex.size=4,
     vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)

plot(g, rescale = FALSE, ylim=c(1,4),xlim=c(-17,24), asp = 0)