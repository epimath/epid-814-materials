library(cluster)
library(dbscan)

#### Data sets ####

# Start with some pre-made/fake data
data("moons") # useful one from DBSCAN
plot(moons)
data("DS3") # also nice one from DBSCAN, a bit reminiscent of some of Jiale's examples
plot(DS3)
data(ruspini)
plot(ruspini)
# other data sets to try - make data with normally distributed sample stuff


# Twitter data
load('conspiracydata.Rdata')
# load('airlinedata.Rdata')
# tweetdata = tweetdata[tweetdata$keyword!="las",]
# tweetdata = tweetdata[tweetdata$keyword!="den",]
# tweetdata = tweetdata[tweetdata$keyword!="sea",]
minidata = tweetdata[,c("followers_count", "friends_count", "listed_count", "statuses_count", "favourites_count")]
scaledata = scale(minidata)


pairs(minidata, col = tweetdata$keyword,
      lower.panel = NULL,
      cex.labels=1, pch=19, cex = 0.7)
legend(x = 0, y = 0.6, cex = 0.6,
       legend = unique(tweetdata$keyword),
       fill = unique(tweetdata$keyword))


#### Clustering! ####

data = moons

# K-Means
numclust = 4
kmeansres = kmeans(x = data, centers = numclust)

kmeansres$centers
kmeansres$size
kmeansres$withinss

clusplot(data, kmeansres$cluster, main='Cluster Visualization',
         color=TRUE, shade=TRUE, labels=numclust, lines=0)
pairs(data, col = kmeansres$cluster, lower.panel = NULL, cex.labels=1, pch=19, cex = 0.5)


# K-medioids (PAM)
pamres = pam(data,4)
clusplot(data, pamres$cluster, main='Cluster Visualization',
         color=TRUE, shade=TRUE, labels=numclust, lines=0)
pairs(data, col = pamres$cluster, lower.panel = NULL, cex.labels=1, pch=19, cex = 0.5)


# Hierarchical
hierres=agnes(data,diss=FALSE,metric="euclidian")
plot(hierres, main='Dendrogram') ## dendrogram

hierres4 = cutree(hierres, k=4) # cut tree into 4 clusters

clusplot(data, hierres4, main='Cluster Visualization',
         color=TRUE, shade=TRUE, labels=numclust, lines=0)

pairs(data, col = hierres4, lower.panel = NULL, cex.labels=1, pch=19, cex = 0.5)



# DBSCAN
dbscanres = dbscan(data, eps = 0.5, minPts = 3)
dbscanres

clusplot(data, dbscanres$cluster, main='Cluster Visualization',
         color=TRUE, shade=TRUE, labels=numclust, lines=0)

pairs(data, col = dbscanres$cluster, lower.panel = NULL, cex.labels=1, pch=19, cex = 0.5)


# Hierarchical DBSCAN
hdbscanres = hdbscan(data, minPts = 3)
hdbscanres

clusplot(data, hdbscanres$cluster, main='Cluster Visualization',
         color=TRUE, shade=TRUE, labels=numclust, lines=0)

pairs(data, col = hdbscanres$cluster, lower.panel = NULL, cex.labels=1, pch=19, cex = 0.5)


