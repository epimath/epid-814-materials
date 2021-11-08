library(cluster)
library(dbscan)
library(ggplot2)
library(dplyr)
library(plotly)
library(rvest) # this is just for web-scraping a dataset
library(nhanesA) # for loading NHANES data


###################
#### Data sets ####
###################


### Start with some pre-made/fake data ###

data("moons") # useful one from DBSCAN
plot(moons)
data("DS3") # also nice one from DBSCAN
plot(DS3)
data(ruspini)
plot(ruspini)


### State COVID Hospitalization Data ###

# This data is a table of COVID+ patients, COVID+ ICU patients, and bed occupancy for hospitals across the state
# In other words, a 3D data set (so we see an example with more than 2 dimensions)
MICovid = read_html("https://www.michigan.gov/coronavirus/0,9753,7-406-98159-523641--,00.html")
MICovid.tables = html_nodes(MICovid, "table")
MICovid.data = html_table(MICovid.tables[[6]])
# MICovid.data = MICovid.data[,2:4] # drop the hospital names column for now---do this when we run the clustering really

# The original data treats a lot of the columns as character vectors, so 
# convert to numeric---note you have to strip the "," and "%" signs
MICovid.data$`COVID-19 Patients` = as.numeric(sub(",","",MICovid.data$`COVID-19 Patients`))
MICovid.data$`COVID-19 Patients in ICU` = as.numeric(sub(",","",MICovid.data$`COVID-19 Patients in ICU`))
MICovid.data$`Bed Occupancy %` = as.numeric(sub("%","",MICovid.data$`Bed Occupancy %`))/100

# Also trim the last row since it's the total
MICovid.data = head(MICovid.data, -1)

max(MICovid.data$`COVID-19 Patients in ICU`)
max(MICovid.data$`COVID-19 Patients`)

ggplot(MICovid.data) + 
  geom_point(aes(x = `COVID-19 Patients`, y = `COVID-19 Patients in ICU`, color = `Bed Occupancy %`))

plot_ly(MICovid.data, x = ~`COVID-19 Patients`, y = ~`COVID-19 Patients in ICU`, z = ~`Bed Occupancy %`, marker = list(color = ~`Bed Occupancy %`, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>% 
  add_markers() 



### NHANES Data ###

## Decide data to explore

# explore a category: 
# Demographics (DEMO) - Dietary (DIET) - Examination (EXAM) - Laboratory (LAB) - Questionnaire (Q)

## Lab data sets 
nhanesTables('Q', 2017) 
nhanesTableVars('LAB', 'PFAS_J')# pull variables list from one of the categories

## Perfluoroalkyl and Polyfluoroalkyl Substances 2017 NHANES
nhanesdata = nhanes('PFAS_J')
# remove comment codes and subsample weight columns
nhanesdata = select(nhanesdata, -WTSB2YR, -LBDPFDEL, -LBDPFHSL, -LBDMPAHL, -LBDPFNAL, -LBDPFUAL, -LBDNFOAL, -LBDBFOAL, -LBDNFOSL, -LBDMFOSL) 

plot_ly(nhanesdata, x = ~`LBXPFDE`, y = ~`LBXPFNA`, z = ~`LBXMPAH`, marker = list(color = ~`LBXPFHS`, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>% 
  add_markers() 


## Complete Blood Count with 5-Part Differential 2017 NHANES
nhanesdata = nhanes('CBC_J')

## Lead, Cadmium, Total Mercury, Selenium, & Manganese
nhanesdata = nhanes('PBCD_J')   # Lead, Cadmium, Total Mercury, Selenium, & Manganese - Blood 2017 NHANES
colSums(nhanesdata)
nhanesdata = select(nhanesdata, -LBDBPBLC, -LBDBSELC, -LBDBMNLC) # remove comment columns that are largely unused



## Questionnaire data sets
nhanesTables('Q', 2015) # explore a category: Demographics (DEMO) - Dietary (DIET) - Examination (EXAM) - Laboratory (LAB) - Questionnaire (Q)
nhanesTableVars('Q', 'SXQ_I') # pull variables list from one of the categories

## Depression screener
nhanesdata = nhanes('DPQ_J') # depression screener 2017 NHANES

## Sexual behavior questionnaire
nhanesdata = nhanes('SXQ_I') 
nhanesdata[is.na(nhanesdata)] = -1 # this one has too many NA's because of skip patterns, so omiting the NA's straight out takes out all the rows

## Cardiovascular Health
nhanesdata = nhanes('CDQ_J')
nhanesdata[is.na(nhanesdata)] = -1 # this one has too many NA's because of skip patterns, so omiting the NA's straight out takes out all the rows




## Process data
nhanesdata = na.omit(nhanesdata) # drop NA rows
nhanesdata = select(nhanesdata, -SEQN) # drop respondent ID column






#####################
#### Clustering! ####
#####################

# data = DS3
# data = MICovid.data[,2:4] # trim off hospital names
# nhanesdata = select(nhanesdata, -`LBXBFOA`)
data = nhanesdata


# K-Means
set.seed(7)
numclust = 4
kmeansres = kmeans(x = data, centers = numclust)

kmeansres$centers # center locartions
kmeansres$size #cluster sizes
kmeansres$withinss #within cluster sum of squares 
kmeansres$cluster # cluster membership vector

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
dbscanres = dbscan(data, eps = 5, minPts = 3) # minpoints often set to dim + 1
dbscanres

clusplot(data, dbscanres$cluster, main='Cluster Visualization',
         color=TRUE, shade=TRUE, labels=numclust, lines=0)

pairs(data, col = dbscanres$cluster, lower.panel = NULL, cex.labels=1, pch=19, cex = 0.5)


# Hierarchical DBSCAN
hdbscanres = hdbscan(data, minPts = 3) # minpoints often set to dim + 1
hdbscanres

clusplot(data, hdbscanres$cluster, main='Cluster Visualization',
         color=TRUE, shade=TRUE, labels=numclust, lines=0)

pairs(data, col = hdbscanres$cluster, lower.panel = NULL, cex.labels=1, pch=19, cex = 0.5)








# Twitter data - do this later
# load('conspiracydata.Rdata')
# load('airlinedata.Rdata')
# tweetdata = tweetdata[tweetdata$keyword!="las",]
# tweetdata = tweetdata[tweetdata$keyword!="den",]
# tweetdata = tweetdata[tweetdata$keyword!="sea",]
# minidata = tweetdata[,c("followers_count", "friends_count", "listed_count", "statuses_count", "favourites_count")]
# scaledata = scale(minidata)


# pairs(minidata, col = tweetdata$keyword,
#       lower.panel = NULL,
#       cex.labels=1, pch=19, cex = 0.7)
# legend(x = 0, y = 0.6, cex = 0.6,
#        legend = unique(tweetdata$keyword),
#        fill = unique(tweetdata$keyword))
