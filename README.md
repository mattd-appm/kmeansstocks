# kmeansstocks
K-means Clustering for Diversified Stock Portfolios

## Description
In this project, I used K-means clustering on stock data from 1/2/2015 to 7/2/2020. In particular, I used the stock movement each day as a variable where positive movement=1 and all other movement=0. I used the elbow method to determine the optimal number of clusters (k=40), euclidean distance and used the kmeans clustering function in R. The overall objectives for this project were to discover novel stock relationships, group similar stocks for aggregate analysis, and develop a diversified portfolio for investing without domain expertise.

## Files
#1.)CE-paper.docx
Contents:
Introduction
Data 
Data Wrangling
K-Means Clustering
Model Selection
Methods
Results

#2.)kmeans.r
R code
Dependencies:
ggplot2
dplyr
reshape2
ggfortify

#3.)kmeans.rdata
workspace that includes results from kmeans clustering to save time

#4.)cluster-results.txt
6335 rows corresponding to each stock
2 columns of the stock symbol and corresponding cluster (ordered ascending with clusters first and alphabetically with symbols)
