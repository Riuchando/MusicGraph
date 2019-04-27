rm(list = ls())


library("plyr")
library("dplyr")
library("data.table")
library("reshape")
library("igraph")

# http://stackoverflow.com/questions/17223308/fastest-way-to-count-occurrences-of-each-unique-element
f6 = function(x) {
  data.table(x)[, .N, keyby = x]
}
#http://stackoverflow.com/questions/22870198/is-there-a-more-efficient-way-to-replace-null-with-na-in-a-list
nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}
####need to do these three lines of code
# df2=data.frame(withTags$Artist ,  withTags$Tag.5)
# colnames(df2) <- c("Artist", "Tag")
# df1<- rbind(df1,df2)
multitosingle <- function(origin, start, end) {
  acc = na.omit(data.frame(origin[[1]], origin[[start]]))
  colnames(acc) <- c("Artist", "Tag")

  for (i in (start + 1):end) {
    df2 = na.omit(data.frame(origin[[1]], origin[[i]]))
    #print(df2)
    colnames(df2) <- c("Artist", "Tag")
    acc <- rbind(acc, df2)
  }
  return(acc)
}
##need to clean spaces
# withTags$Tag.1[as.character(withTags$Tag.1) == " "] <- NA
# withTags$Tag.2[as.character(withTags$Tag.2) == " "] <- NA
# withTags$Tag.3[as.character(withTags$Tag.3) == " "] <- NA
#....
cleanSpaces <- function(origin, start, end) {
  for (i in start:end) {
    origin[[i]][as.character(origin[[i]]) == " "] <- NA
  }
  return(origin)
}
adjacency_from_agg <- function(aggList) {
  # combn(aggList$Artist[[1]],2)
  # combn(agglist$Artist[[4]],2)
  # other$Artist[[1]]
  # note: Because of how R works, the combn is one really long row instead of 2 col with a lot of rows, 
  # I could do a colbind and T at the end, but I'm lazy
  acc = t(combn(aggList$Artist[[1]], 2))
  for (i in 2:length(aggList$Artist)) {
    tmp = t(combn(aggList$Artist[[i]], 2))
    acc = rbind(acc, tmp)
  }
  return(acc)
}

scrobbles <- read.csv(file = "scrobbles.tsv", sep = "\t")
bootstrap <- read.csv(file = "bootstraps.tsv", sep = "\t")

artistscounts <- as.data.frame(f6(scrobbles$artist.name))
sparkArtists <- createDataFrame(sqlContext, artistscounts)
write.csv(file = "ArtistCounts.csv", x = artistscounts)

#plyr::count(scrobbles, vars = scrobbles$artist.name)
dplyr::group_by(scrobbles, as.character(scrobbles$artist.name))
#scrobbles_by_plays <- scrobbles %>%
#group_by(artist.name)
# summarise(sum_of_occurence=,
#          n=n())



#after processing with last fm webscraper
withTags <- read.csv("lastfm.csv")


summary(withTags)
# cleaning withTags
withTags$LN <- NULL
withTags <- cleanSpaces(withTags, 3, 7)
# http://www.r-statistics.com/2012/01/aggregation-and-restructuring-data-from-r-in-action/
adjlist <- melt(withTags, id = c("Artist", "Count"))
adjlist$variable <- NULL
adjlist$value <- as.character(adjlist$value)
# adjlist <- multitosingle(withTags, 3,7)

# for now ignore subgenres with only 1 tag
morePreProcess = f6(adjlist$value)
bucket = morePreProcess[morePreProcess$N == 1]
setdiff(as.character(adjlist$Tag), as.character(bucket$x))

# there needs to be a better way of doing this....
adjlist$value[as.character(adjlist$value) %in% as.character(bucket$x)] <- NA
adjlist = na.omit(adjlist)
# for every unique tag, connect the artist related to them
length(unique(adjlist$Artist))
length(unique(withTags$Artist))
adjlist$Artist <- as.character(adjlist$Artist)
# adjlist$Tag <- as.character(adjlist$Tag)
# turn Adjacency list into graph
other <- aggregate(Artist ~ value, c, data = adjlist)

alist <- adjacency_from_agg(other)
# the following line of code is breaking on me, let's split this program in two
write.csv(file = "finalAdjlist.csv", x = alist)
# artistGraph =graph.adjlist(adjlist= alist,duplicate = T)


# graph Graph