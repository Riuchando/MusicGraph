#Author: Stephen Kinser
#based on an example from 
#title: "Introduction to cyREST"
#author: "Keiichiro Ono"
#date: "6/15/2015"
rm(list=ls())
library(RJSONIO)
library(igraph)
library(httr)
library(Unicode)
library(RColorBrewer)

# Basic settings
port.number = 1234
base.url = paste("http://localhost:", toString(port.number), "/v1", sep="")

print(base.url)


version.url = paste(base.url, "version", sep="/")
cytoscape.version = GET(version.url)
cy.version = fromJSON(rawToChar(cytoscape.version$content))
print(cy.version)

setwd("C:/Users/Stephen/Documents/GitHub/MusicGraph")
# 1. Create simple directed graph with Barabasi-Albert model
#graph1 <- barabasi.game(200)
adjlist <- read.csv("finalAdjlist.csv", encoding = "UTF-8")
withTags <- read.csv("lastfm.csv",encoding = "UTF-8")
adjlist$X <- NULL
adjlist$V1 <- as.character(adjlist$V1)
adjlist$V2 <- as.character(adjlist$V2)
adjlist$V1[adjlist$V1 == "M|O|O|N"] <- "M/O/O/N"
adjlist$V2[adjlist$V2 == "M|O|O|N"] <- "M/O/O/N"
graph <- graph.data.frame(adjlist, directed = F)
graph <- simplify(graph, remove.multiple = T, remove.loops = T)
# 2. Calculate some statistics and assign then to the graph
#graph1$name = "Scale-Free Network (BA Model)"
#graph1$density = graph.density(graph1)
graph$name = "Stephen 5/2015- 6/2015"
graph$density = graph.density(graph)
withTags$Artist <- as.character(withTags$Artist)
withTags$Artist[withTags$Artist == "M|O|O|N"] <- "M/O/O/N"
withTags$Tag.1<- as.character(withTags$Tag.1)
#V(graph)[1]$name

#withTags[which(withTags$Artist=="Weezer"),"Count"]
#withTags[which(withTags$Artist==V(graph)[1]$name),"Count"]
#length(V(graph)$name)
withTags[which(withTags$Artist=="Björk"),]
for(i in 1:length(V(graph)$name)){
  V(graph)[i]$count <-withTags[which(withTags$Artist==V(graph)[i]$name),"Count"]
}

for(i in 1:length(V(graph)$name)){
  tempTag <-withTags[which(withTags$Artist==V(graph)[i]$name),"Tag.1"]
  if(tempTag== "classic rock" || tempTag == "rock n roll" || 
     tempTag == "rockabilly" || tempTag == "blues" ||
     tempTag == "blues rock"|| tempTag == "hard rock" || 
     tempTag == "rock"){
    V(graph)[i]$color <- "#0099FF"
 
  }
  else if(tempTag == "progressive metal" || tempTag == "progressive black metal"||
          tempTag == "progressive rock" || tempTag == "art rock"){
    V(graph)[i]$color <- "#6699FF"
  }
  else if(tempTag == "alternative" || tempTag == "alternative rock" ||
          tempTag == "grunge"){
    V(graph)[i]$color <- "#666699"
  }
  else if(tempTag == "surf rock" || tempTag == "surf"){
    V(graph)[i]$color <- "#0099CC"
  }
  else if(tempTag == "video game music"|| tempTag == "electronic"|| 
          tempTag == "electro-swing"|| tempTag == "idm" ||
          tempTag == "glitch"|| tempTag == "synthwave"||
          tempTag == "synthwave"|| tempTag == "dance" ||
          tempTag == "electro"|| tempTag == "house"||
          tempTag == "dubstep"|| tempTag == "drum and bass"||
          tempTag == "electro house" || tempTag == "c64"|| 
          tempTag == "chiptune" || tempTag == "chillwave" || tempTag == "mpb"){
    V(graph)[i]$color <- "#FFFF99"
  }
  else if(tempTag == "latin" ||tempTag == "tango"||
          tempTag == "bossa nova"){
    V(graph)[i]$color <- "##66FF99"
  }
  else if(tempTag == "psychedelic" ||tempTag == "stoner rock" || 
          tempTag == "psychedelic rock" ||tempTag == "jam" ||
          tempTag == "psytrance"){
    V(graph)[i]$color <- "#9966FF"
  }
  else if (tempTag == "jazz"|| tempTag == "lounge"||
           tempTag == "swing"){
    V(graph)[i]$color <- "#66FFFF"
  }
  else if (tempTag == "post-hardcore" || tempTag == "punk" || 
           tempTag == "post-punk"|| tempTag == "hardcore" ||
           tempTag == "punk rock" || tempTag == "gypsy punk" ||
           tempTag == "emo"){
    V(graph)[i]$color <- "#9999FF"
  }
  else if(tempTag == "soundtrack"|| tempTag == "instrumental"||
          tempTag == "minimalism" || tempTag == "post-rock"||
          tempTag == "contemporary classical" || tempTag == "downtempo"){
    V(graph)[i]$color <- "#FF5050"
  }
  else if(tempTag == "sludge"|| tempTag == "thrash metal"||
          tempTag == "death metal"|| tempTag == "brutal death metal"||
          tempTag == "doom metal"|| tempTag == "heavy metal" || 
          tempTag == "black metal" || tempTag == "metal" || 
          tempTag == "funk metal"){
    V(graph)[i]$color <- "#000066"
  }
  else if(tempTag == "reggae" || tempTag == "ska" || tempTag == "dub"){
    V(graph)[i]$color <- "#00CC66"
  }
  else if (tempTag == "indie" || tempTag == "indie pop"||
           tempTag == "indie rock" || tempTag == "lo-fi"){
    V(graph)[i]$color <- "#CCFFFF"
  }
  else if(tempTag == "hip-hop"||tempTag == "hip hop" ||
          tempTag == "turntablism" ||tempTag == "rap" ||
          tempTag == "glitch-hop"|| tempTag == "trip-hop"){
    V(graph)[i]$color <- "#CC00CC"
  }
  else if(tempTag == "classical"|| tempTag == "classical guitar"||
          tempTag == "harp"){
    V(graph)[i]$color <- "#FF3030"
  }
  else if (tempTag == "garage rock"||tempTag == "garage punk"){
    V(graph)[i]$color <- "#0033CC"
  }
  else if(tempTag == "metalcore" || tempTag == "nu metal"){
    V(graph)[i]$color <- "#000099"
  }
  else if(tempTag == "new wave"){
    V(graph)[i]$color <- "#FFFFCC"
  }
  else if(tempTag == "folk"|| tempTag == "singer-songwriter"){
    V(graph)[i]$color <- "#339966"
  }
  else if (tempTag == "country"){
    V(graph)[i]$color <- "#006600"
  }
  else if(tempTag == "female vocalists" || tempTag == "pop"||
          tempTag == "comedy"||tempTag == "oldies"||
          tempTag == "pop punk" || tempTag == "disco" ||
          tempTag == "60s" || tempTag == "britpop" ||
          tempTag == "dream pop"|| tempTag == "eurodance" ||
          tempTag == "glam rock" || tempTag == "covers"){
    V(graph)[i]$color <- "#FF99FF"
  }
  else if(tempTag == "musicals"|| tempTag == "broadway"||
          tempTag == "dark cabaret" || tempTag == "ragtime" ||
          tempTag == "a cappella" || tempTag == "acapella" ||
          tempTag == "punk cabaret"){
    V(graph)[i]$color <- "#FFCCFF"
  }
  else if(tempTag == "experimental" || tempTag == "noise rock"|| 
          tempTag == "noise" || tempTag == "watermelon" ||
          tempTag == "ninja tune" || tempTag == "microtonal"||
          tempTag == "under 2000 listeners" || tempTag == "ambient"||
          tempTag == "industrial"){
    V(graph)[i]$color <- "#663300"
  }
  else if (tempTag == "soul"|| tempTag == "funk"){
    V(graph)[i]$color <- "#009900"
  }
  else if(tempTag == "french"|| tempTag == "japanese"||
          tempTag == "canadian" || tempTag == "australian"){
    V(graph)[i]$color <- "#FFCCCC"
  }
  else{
    V(graph)[i]$color <- "#FFFFFF"
    print(tempTag)  }
  
}

V(graph)$count
V(graph)$degree <- igraph::degree(graph)
V(graph)$closeness <- closeness(graph)
V(graph)$betweenness <- betweenness(graph)
V(graph)$page_rank <- page.rank(graph)$vector
V(graph)$community <- label.propagation.community(graph)$membership
V(graph)["Open Mind"]
E(graph)$betweenness <- edge.betweenness(graph)

default.style.url = paste(base.url, "styles/default", sep="/")
GET(url=default.style.url)

#Here is the sample code to generate new Style:
style.name = "Music hard coded"

# Defaults
def.node.color <- list(
  visualProperty = "NODE_FILL_COLOR",
  value = "#eeeeee"
)

def.node.border.width <- list(
  visualProperty = "NODE_BORDER_WIDTH",
  value = 0
)

def.node.size <- list(
  visualProperty = "NODE_SIZE",
  value = 10
)

def.node.transparency = list(
  visualProperty="NODE_TRANSPARENCY",
  value = 200
)

def.node.label.transparency = list(
  visualProperty="NODE_LABEL_TRANSPARENCY",
  value = 100
)

def.edge.width <- list(
  visualProperty="EDGE_WIDTH",
  value=3
)

defaults <- list(
  def.node.color,
  def.node.border.width,
  def.node.size,
  def.node.transparency,
  def.edge.width)

# Visual Mappings
min.betweenness = min(V(graph)$betweenness)
max.betweenness = max(V(graph)$betweenness)

mappings = list()

colors <- brewer.pal(3,"Dark2")
essvals <- c("E", "N", "u")
discrete.mappings = list()
for(i in 1:length(colors)) {
  discrete.mappings[[i]] <- list(key = essvals[i], value = colors[i])
}

node.color = list(
  mappingType="passthrough",
  mappingColumn="color",
  mappingColumnType="String",
  visualProperty="NODE_FILL_COLOR",
  map = discrete.mappings
)

node.label = list(
  mappingType="passthrough",
  mappingColumn="name",
  mappingColumnType="String",
  visualProperty="NODE_LABEL"
)
node.size = list(
  mappingType="passthrough",
  mappingColumn="count",
  mappingColumnType="Integer",
  visualProperty="NODE_SIZE"
)

mappings = list(node.color, node.label,node.size)

style <- list(title=style.name, defaults = defaults, mappings = mappings)
style.JSON <- toJSON(style)

style.url = paste(base.url, "styles", sep="/")
POST(url=style.url, body=style.JSON, encode = "json")

# Load utility functions
#setwd("C:/Users/Stephen/Documents/GitHub/cy-rest-R")
source("C:/Users/Stephen/Documents/GitHub/cy-rest-R/utility/cytoscape_util.R")

# Convert to Cytoscape style JSON object
cygraph <- toCytoscape(graph)

# Send it to Cytoscape!
network.url = paste(base.url, "networks", sep="/")
res <- POST(url=network.url, body=cygraph, encode="json")

# Extract SUID of the new network
network.suid = unname(fromJSON(rawToChar(res$content)))

apply.layout.url = paste(
  base.url,
  "apply/layouts/force-directed",
  toString(network.suid),
  sep="/"
)

apply.style.url = paste(
  base.url,
  "apply/styles",
  style.name,
  toString(network.suid),
  sep="/"
)

res <- GET(apply.layout.url)
res <- GET(apply.style.url)

network.image.url = paste(
  base.url,
  "networks",
  toString(network.suid),
  "views/music.png",
  sep="/"
)
print(network.image.url)
