#############################################################
# Data Mining
# Computer Lab Session n°5:
# Association Rule mining Mining with R
#############################################################


###########################
# Association Rule Mining #
###########################

lastfm <- read.csv("~/R/data/lastfm.csv")
head(lastfm)
lastfm[1:20,]
length(lastfm$user) ## 289,955 records in the file
lastfm$user <- factor(lastfm$user)
levels(lastfm$user) ## 15,000 users
levels(lastfm$artist) ## 1,004 artists
library(arules) ## a-rules package for association rules
## Computational environment for mining association rules and
## frequent item sets
## we need to manipulate the data a bit before using arules
## we split the data in the vector x into groups defined in vector f
## in supermarket terminology, think of users as shoppers and artists
## as items bought
playlist <- split(x=lastfm[,"artist"],f=lastfm$user)
## split into a list of users
playlist[1:2]
## the first two listeners (1 and 3) listen to the following bands

## an artist may be mentioned by the same user more than once
## it is important to remove artist duplicates before creating
## the incidence matrix
playlist <- lapply(playlist,unique) ## remove artist duplicates
playlist <- as(playlist,"transactions")
## view this as a list of "transactions"
## transactions is a data class defined in arules
itemFrequency(playlist)
## lists the support of the 1,004 bands
## number of times band is listed to on the playlist of 15,000 users
## computes relative frequency of artist mentioned by the 15,000 users

win.graph(800,600,10)
itemFrequencyPlot(playlist,support=.08,cex.names=1.5)
## plots the item frequencies (only bands with > % support)

## Finally, we build the association rules
## only associations with support > 0.01 and confidence > .50
## this rules out rare bands
musicrules <- apriori(playlist,parameter=list(support=.01,
                                              confidence=.5))
inspect(musicrules)
## let's filter by lift > 5.
## Among those associations with support > 0.01 and confidence > .50,
## only show those with lift > 5
inspect(subset(musicrules, subset=lift > 5))

## lastly, order by confidence to make it easier to understand
inspect(sort(subset(musicrules, subset=lift > 5), by="confidence"))



library(arulesViz)
win.graph(800,600,10)
plot(musicrules)
## try: 
sel <- plot(musicrules, interactive=TRUE)
## Two-key plot is a scatterplot with shading = "order"
win.graph(800,600,10)
plot(musicrules, shading="order", control=list(main = "Two-key plot"))
## the following techniques work better with fewer rules
subrules <- musicrules[quality(musicrules)$confidence > 0.5]
## 2D matrix with shading
plot(subrules, method="matrix", measure="lift")
plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE))
## 3D matrix
plot(subrules, method="matrix3D", measure="lift")
plot(subrules, method="matrix3D", measure="lift", control=list(reorder=TRUE))
## matrix with two measures
plot(subrules, method="matrix", measure=c("lift", "confidence"))
plot(subrules, method="matrix", measure=c("lift", "confidence"),
     control=list(reorder=TRUE))
## try: plot(subrules, method="matrix", measure="lift", interactive=TRUE,
## control=list(reorder=TRUE))
## grouped matrix plot
plot(musicrules, method="grouped")
plot(musicrules, method="grouped", control=list(k=30))
## try: sel <- plot(rules, method="grouped", interactive=TRUE)
## graphs only work with very few rules
subrules2 <- sample(musicrules, 10)
plot(subrules2, method="graph")
plot(subrules2, method="graph",
     control=list(type="items"))
## try: plot(subrules2, method="graph", interactive=TRUE)
## try: plot(subrules2, method="graph", control=list(engine="graphviz",
## type="items"))
## parallel coordinates plot
plot(subrules2, method="paracoord")
plot(subrules2, method="paracoord", control=list(reorder=TRUE))
## use iplots (experimental)
## try: sel <- plot(rules, method="iplots", interactive=TRUE)
## for itemsets
itemsets <- eclat(lastfm, parameter = list(support = 0.02))
plot(itemsets, method="paracoord", control=list(alpha=.5, reorder=TRUE))
plot(itemsets, method="graph")

