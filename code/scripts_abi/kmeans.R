setwd("/Users/abieast/Documents/Imperial/Translational_Data_Science/Project")

library(tidyverse)
library(cluster)
library(factoextra)
library(FactoMineR)

source("Git_Repo/code/utility_functions/FAMD_plots_utility.R")
source("Git_Repo/code/utility_functions/colors_themes_utility.R")
source("Git_Repo/code/utility_functions/clustering_utility.R")

################################################################################
# LOADING AND SCALING DATA
################################################################################

biobank <- readRDS("Data/scaled_multi_morbid.rds")
biobank_numeric <- sapply(biobank, as.numeric)
biobank_numeric <- scale(biobank_numeric)

################################################################################
# Choosing the number of clusters for kmeans
################################################################################

library(clusterCrit)
n_classes=2:8

cluster_crit_df=as.data.frame(matrix(0,nrow=length(n_classes),ncol=3))
cluster_crit_df[,1]=n_classes
colnames(cluster_crit_df)=c("n_classes","Cal_Har","Silhouette")

# Different numbers of centers
for (k in 1:length(n_classes)) {
  
  kmeans_biobank=kmeans(biobank_numeric,centers=n_classes[k])
  cluster_crit_df[k,2:3]=unlist(intCriteria(traj=as.matrix(biobank_numeric),
                                            part=kmeans_biobank$cluster,c("Calinski_Harabasz","Silhouette")))
}

## CH is largest with 2 clusters

################################################
# CH criterion plot
################################################

n_centers=2:8
CH_criterion=rep(0,length(n_centers))

## Different numbers of centers
for (k in 1:length(n_centers)) {
  
  clusters_kmeans_biobank=kmeans(biobank_numeric, center = n_centers[k])
  CH_criterion[k]=intCriteria(traj=biobank_numeric,
                              part=clusters_kmeans_biobank$cluster,crit="Calinski_Harabasz")[[1]]
}

## CH criterion plot

kmeans_CH_plot = ggplot(data=data.frame(n_centers,CH_criterion)) + geom_line(aes(x=n_centers,y=CH_criterion),size=0.8,color="red") + theme_jh +
  ggtitle("Calinski Harabasz criterion plot")

svg("Git_Repo/code/results_abi/kmeans_CH_plot.svg", width=10,height=10)
kmeans_CH_plot
dev.off()

################################################
# Elbow Plot
################################################

## Different numbers of centres

n_centers_elbow=2:15
tot_withinss=rep(0,length(n_centers))

for (k in 1:length(n_centers_elbow)) {
  
  clusters_kmeans_biobank=kmeans(biobank_numeric, center = n_centers_elbow[k])
  tot_withinss[k]=kmeans(biobank_numeric, center = n_centers_elbow[k])$tot.withinss
}

## Make plot using factoextra package

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(biobank_numeric, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares", xlim = c(0, 15), ylim = c(650000, 850000), main = "Kmeans Elbow Plot")

svg("Git_Repo/code/results_abi/kmeans_elbow.svg", width=10,height=10)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares", xlim = c(0, 15), ylim = c(650000, 850000), main = "Kmeans Elbow Plot")
dev.off()

################################################
# Silhouette Plot
################################################

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

###### Lots of warnings: did not converge in 10 iterations, Quick-TRANSfer stage steps exceeded maximum (= 609550)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE,
     ylim = c(0.035,0.07),
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

svg("Git_Repo/code/results_abi/kmeans_silhouette.svg", width=10,height=10)
plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE,
     ylim = c(0.035,0.07),
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")
dev.off()

#No idea how many clusters!!!

#Next steps: run kmeans for optimal number of clusters
#Plot using PCA

################################################################################
# KMEANS WITH OPTIMAL NUMBER OF CLUSTERS
################################################################################

## Number of clusters = ???
kmeans_optimal <- kmeans(biobank_numeric, centers = ???, nstart = 25)

## Visualise the clusters-package does PCA
fviz_cluster(kmeans_optimal, biobank_numeric)
