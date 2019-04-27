rm(list = ls())
library("igraph")

library(RCytoscape)
library(RJSONIO)
library(httr)
library(Unicode)
setwd("C:/Users/Stephen/Documents/GitHub/MusicGraph")
adjlist <- read.csv("finalAdjlist.csv", encoding = "UTF-8")
adjlist$X <- NULL
adjlist$V1 <- as.character(adjlist$V1)
adjlist$V2 <- as.character(adjlist$V2)
adjlist$V1[adjlist$V1 == "M|O|O|N"] <- "M/O/O/N"
adjlist$V2[adjlist$V2 == "M|O|O|N"] <- "M/O/O/N"
graph <- graph.data.frame(adjlist, directed = F)
#g.tca <- simplify(graph, remove.multiple = T, remove.loops = T)
#g.tca$name = "Stephen 5/2015- 6/2015"
#png("IMightNeedABiggerComputer.png")
#plot(g.tca)
#dev.off()
#this one is too slow
#cliques(g.tca)
gD.cyt <- igraph.to.graphNEL(graph)

gD.cyt <- initNodeAttribute(gD.cyt, 'degree', 'numeric', 0)
gD.cyt <- initNodeAttribute(gD.cyt, 'betweenness', 'numeric', 0)
gD.cyt <- initEdgeAttribute(gD.cyt, "weight", 'integer', 0)
gD.cyt <- initEdgeAttribute(gD.cyt, "similarity", 'numeric', 0)
cy <- CytoscapeConnection(host = "localhost", rpcPort = 9000)

pluginVersion(cy)
gDCW <- new.CytoscapeWindow("Music Graph", graph = gD.cyt, overwriteWindow = TRUE)
? new.CytoscapeWindow


cw <- CytoscapeWindow('new.demo', new('graphNEL'))
