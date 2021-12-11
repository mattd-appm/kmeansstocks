#Dependencies
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggfortify)

set.seed(2021)

#Load data (Kaggle)
raw.dat = dat = read.csv("~/fh_5yrs.csv")

#Exploratory data analysis
str(dat)
summary(dat)
head(dat)

#Number of unique dates
length(unique(dat$date))

#Number of unique stocks
length(unique(dat$symbol))

#Add diff variable to track daily stock movement
dat = mutate(dat,diff = close - open)

#Check data transformation to make diff variable tractable
summary(dat$diff)
head(dat$diff)
max(dat$diff)

#Histograms of diff and scale(diff) 
hist(dat$diff,
     xlab='close-open price',
     main='Histogram of diff Variable')
hist(scale(dat$diff),
     xlab='scale(diff)',
     main='Histogram of scale(diff) Variable')
max(scale(dat$diff))


#Create movement variable which is 1 if stock has positive movement in that day
#and 0 if stock has negative or no movement in that day
dat$move = 0
dat$move[dat$close > dat$open] = 1
table(dat$move)

#Use only stock name, date, and movement as variables
colnames(dat)
dat = select(dat,symbol,date,move)
head(dat)

#Reshape data so each day is a column
dat.cast = dcast(dat,symbol~date)
dim(dat.cast)

#Handling missing values
sum(is.na(dat.cast))

#Replace NAs with mean stock movement for that day
for (i in 2:ncol(dat.cast)){
  dat.cast[,i][is.na(dat.cast[,i])] = mean(dat.cast[,i],na.rm = TRUE)
}

#Check for missing values
sum(is.na(dat.cast))

## Choosing optimal K 
#Elbow method
#Compute sum of squares within all clusters 
wss = function(k){
  cluster = kmeans(dat.cast[,-1], k)
  cat("Evaluated k =",k,'\n')
  return(cluster$tot.withinss)
}

#Maximum number of clusters
max.k = 40

#Compute sum of squares within all clusters over k from 2 to max.k
wss.overk = sapply(2:max.k,wss)
elbow = data.frame('k' = 2:max.k,'within.ss' = wss.overk)

#Elbow method plot
ggplot(data = elbow,mapping = aes(x=k, y = within.ss)) +
  geom_point() +
  geom_line() +
  geom_point(data = elbow[18,],col='red') +
  labs(x = 'Number of Clusters',
       y = 'Within Sum of Squares',
       title = 'Elbow Method')

#Pick optimal k
opt.k = 19

#K-means cluster with 5 clusters for visualization
k.vis = kmeans(dat.cast[,-1],5)
autoplot(k.vis, dat.cast, frame = TRUE)

#K-means clustering with optimal clusters
k.fit = kmeans(dat.cast[,-1],opt.k, nstart=10)

#Add clusters to dataset
dat.cast = mutate(dat.cast, clust = k.fit$cluster)

#Check size of clusters
k.fit$size
barplot(k.fit$size,main='Cluster Size',xlab='Cluster')

#Reproduce elbow plot up to 60 clusters
wss.overk2 = sapply((max.k+1):60,wss)
elbow=data.frame('k' = 2:60,'within.ss' = c(wss.overk,wss.overk2))

#Elbow method plot
ggplot(data = elbow,mapping = aes(x=k, y = within.ss)) +
  geom_point() +
  geom_line() +
  geom_point(data = elbow[39,],col='red') +
  labs(x = 'Number of Clusters',
       y = 'Within Sum of Squares',
       title = 'Elbow Method with K<= 60')

#New optimal clusters
opt.k = 40

#K-means clustering with optimal clusters
k.fit2 = kmeans(dat.cast[,-1],opt.k, nstart=10)
k.fit2$size
barplot(k.fit2$size,main='Cluster Size with K=40',xlab='Cluster')

#Add clusters to datasets
dat.cast = mutate(dat.cast, clust = k.fit2$cluster)

#Checking biggest and smallest clusters
max(k.fit2$size)
k.fit2$size[k.fit2$size<50]

#Evaluate clusters
#List stocks by symbol in each cluster
list.stocks = function(cluster){
 return (filter(dat.cast, clust == cluster)$symbol)
}

#Cast raw data with open price of each stock, symbol and date as variables
dat = raw.dat
dat = select(dat,date,symbol,open)
dat = dcast(dat,symbol~date)
dat = mutate(dat, clust = k.fit2$cluster)

##Create time series of average opening stock price of cluster
#from 1/2/15 - 7/2/20
time.series = function(cluster){
  dat.clust = filter(dat, clust == cluster)
  cmean = colMeans(dat.clust[,2:(dim(dat.clust)[2]-1)],na.rm=TRUE)
  plt = ggplot(mapping = aes(x = 1:length(cmean), y =cmean)) +
        geom_line() +
        labs(x="Days since 1/2/2015",
             y ="Average Stock Price",
             title = paste("Average Stock Price for Cluster ",
                           as.character(cluster)))
  return (plt)
}
#Example time series
time.series(25)
time.series(3)

#Diversify Portfolio
#Using the kmeans clustering results, randomly select perclust number of stocks
#from each cluster
diversify = function(perclust = 1){
    portfolio = rep(NA,opt.k*perclust)
    i=1
    for (clust in 1:opt.k){
      for (stock in 1:perclust){
        portfolio[i] = sample(list.stocks(cluster=clust),1)
        i = i+1
      }
    }
  return(portfolio)
}
diversify()

#Check sectors and corresponding clusters
en = c('XOM','CVX','COP','NEE','TRP')
dat.cast[dat.cast$symbol %in% en,]$clust

mat = c('SHW','DD','RIO','LYB','IP')
dat.cast[dat.cast$symbol %in% mat,]$clust

ind = c('BA','UNP','WM','FDX','MMM')
dat.cast[dat.cast$symbol %in% ind,]$clust

util = c('DUK','ED','AEP','AWK','BIP','BIPC','NEE')
dat.cast[dat.cast$symbol %in% util,]$clust

hlth = c('UNH','JNJ','VRTX','ISRG','TDOC')
dat.cast[dat.cast$symbol %in% hlth,]$clust

fin = c('BRK-A','BRK-B','JPM','V','VFH','BAC','C','BLK','MS')
dat.cast[dat.cast$symbol %in% fin,]$clust

con.dis = c('AMZN','MCD','NKE','SBUX','TJX','DIS')
dat.cast[dat.cast$symbol %in% con.dis,]$clust

con.stap = c('KO','PG','COST','PEP','EL')
dat.cast[dat.cast$symbol %in% con.stap,]$clust

IT = c('AAPL','MSFT','AMZN','INTC','CSCO','NFLX','FB','GOOG')
dat.cast[dat.cast$symbol %in% IT,]$clust

comm = c('TMUS','CMCSA','FB','GOOG')
dat.cast[dat.cast$symbol %in% comm,]$clust

real.est = c('AMT','SPG')
dat.cast[dat.cast$symbol %in% real.est,]$clust