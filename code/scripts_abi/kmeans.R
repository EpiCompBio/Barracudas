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
# Elbow plot and CH criterion plot
################################################

n_centers=2:8
tot_withinss=rep(0,length(n_centers))
CH_criterion=rep(0,length(n_centers))

# Different numbers of centers
for (k in 1:length(n_centers)) {
  
  clusters_kmeans_biobank=kmeans(biobank_numeric, center = n_centers[k])
  tot_withinss[k]=kmeans(biobank_numeric, center = n_centers[k])$tot.withinss
  CH_criterion[k]=intCriteria(traj=biobank_numeric,
                              part=clusters_kmeans_biobank$cluster,crit="Calinski_Harabasz")[[1]]
}

n_centers_elbow=2:15
for (k in 1:length(n_centers_elbow)) {
  
  clusters_kmeans_biobank=kmeans(biobank_numeric, center = n_centers_elbow[k])
  tot_withinss[k]=kmeans(biobank_numeric, center = n_centers_elbow[k])$tot.withinss
}

svg("Git_Repo/code/results_abi/elbow_cluster_plot.svg",width=10,height=10)
kmeans_CH_plot
dev.off()

#Elbow plot with more values of centers
a<- c(824757.4, 799361.7, 772713.6, 761644.1, 749158.3, 737376.7, 728823.2, 718671.6, 704123.7, 713497.0, 696213.5, 
      691335.1, 690817.2, 680445.9)
b <- c(2:15)
plot(b,a)

kmeans_CH_plot = ggplot(data=data.frame(n_centers,CH_criterion)) + geom_line(aes(x=n_centers,y=CH_criterion),size=0.8,color="red") + theme_jh +
  ggtitle("Calinski Harabasz criterion plot")

#No idea how many clusters!!!

#Next steps: run kmeans for optimal number of clusters
#Plot using PCA
