setwd("/Users/abieast/Documents/Imperial/Translational_Data_Science/Project")

library(tidyverse)
library(cluster)
library(factoextra)

################################################################################
# LOADING AND SCALING DATA
################################################################################

biobank <- readRDS("Data/mm_scaled.rds")
biobank_numeric <- sapply(biobank, as.numeric)
summary(biobank_numeric)

biobank_numeric <- scale(biobank_numeric)

#To get a distance matrix use get_dist
#distance <- get_dist(biobank_numeric)

################################################################################
# KMEANS
################################################################################

## Number of clusters = 2
k2 <- kmeans(biobank_numeric, centers = 2, nstart = 25)
print(k2)
## Visualise the clusters-package does PCA
fviz_cluster(k2, biobank_numeric)

## Elbow plot ##

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(biobank_numeric, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares", xlim = c(0, 15), ylim = c(650000, 950000))

## Silhouette Plot ##

## Package
fviz_nbclust(biobank_numeric, kmeans, method = "silhouette")

## Alternative
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(biobank_numeric, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(biobank_numeric))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15
# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

## Trying with k=4

set.seed(123)
final <- kmeans(biobank_numeric, 4, nstart = 25)
print(final)
fviz_cluster(final, data = biobank_numeric)
fviz_cluster(k2, data = biobank_numeric)
