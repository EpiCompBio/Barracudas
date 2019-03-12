setwd("/Users/abieast/Documents/Imperial/Translational_Data_Science/Project")

library(tidyverse)
library(cluster)
library(factoextra)
library(FactoMineR)

multi_morbid <- readRDS("Data/mm_scaled.rds")

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
# KMEANS WITH OPTIMAL NUMBER OF CLUSTERS (2,3,4)
################################################################################

## Number of clusters = 2 
kmeans_optimal_2 <- kmeans(biobank_numeric, centers = 2, nstart = 25)
## Visualise the clusters-package does PCA
fviz_cluster(kmeans_optimal_2, biobank_numeric)

svg("Git_Repo/code/results_abi/kmeans_2.svg", width=10,height=10)
fviz_cluster(kmeans_optimal_2, biobank_numeric)
dev.off()

## Number of clusters = 3 
kmeans_optimal_3 <- kmeans(biobank_numeric, centers = 3, nstart = 25)
fviz_cluster(kmeans_optimal_3, biobank_numeric)

svg("Git_Repo/code/results_abi/kmeans_3.svg", width=10,height=10)
fviz_cluster(kmeans_optimal_3, biobank_numeric)
dev.off()

## Number of clusters = 4 
kmeans_optimal_4 <- kmeans(biobank_numeric, centers = 4, nstart = 25)
fviz_cluster(kmeans_optimal_4, biobank_numeric)

svg("Git_Repo/code/results_abi/kmeans_4.svg", width=10,height=10)
fviz_cluster(kmeans_optimal_4, biobank_numeric)
dev.off()

################################################
# Means continuous variables by cluster
################################################

##### Function for means by cluster plot #####

library(reshape2)
data=biobank_numeric
classes=as.factor(kmeans_optimal_3$cluster)
color_scale=NULL
custom_theme=theme_jh
title=NULL

mean_by_cluster=function(data,classes,color_scale=NULL,custom_theme=NULL,title=NULL) {
  
  data_and_classes = data.frame(data, classes)
  means_clusters=aggregate(data_and_classes[,-c(ncol(data_and_classes))],
                           by=list(data_and_classes[,ncol(data_and_classes)]),mean)
  
  means_clusters=apply(means_clusters,3,as.numeric)
  
  plot_data=melt(setNames(data.frame(t(means_clusters)[-1,],id=colnames(data)),c(levels(classes),"id")),
                 id.var="id")
  
  colnames(plot_data)[2]="cluster"
  
  mean_clusters_plot=ggplot(plot_data, aes(x=id,y=value,group=cluster,colour=cluster)) +
    geom_point() + geom_line(aes(lty=cluster)) + 
    xlab("Variables") + ylab("Mean") 
  if (!is.null(color_scale)) {
    mean_clusters_plot=mean_clusters_plot + scale_colour_manual(name="Cluster", values = color_scale, labels = levels(classes)) +
      scale_linetype_discrete(name="Cluster",labels = color_scale)
  }
  if (!is.null(title)) {
    mean_clusters_plot = mean_clusters_plot + ggtitle(title)
  }
  mean_clusters_plot=mean_clusters_plot + custom_theme + 
    theme(axis.text.x = element_text(angle = 90))
  return(mean_clusters_plot)
}

##### Mean by cluster plot, k=2 #####

kmeans_mean_by_cluster=mean_by_cluster(data=biobank_numeric, classes=as.factor(kmeans_optimal_2$cluster),
                                                                       color_scale=NULL,custom_theme=theme_jh,title=NULL)


svg(filename="Git_Repo/code/results_abi/kmeans_by_cluster_plot.svg",width=10,height=10)
print(kmeans_mean_by_cluster)
dev.off()

##### Mean by cluster plot, k=3 #####
##### Doesn't work currently #####

kmeans_mean_by_cluster_3=mean_by_cluster(data=biobank_numeric, classes=as.factor(kmeans_optimal_3$cluster),
                                       color_scale=NULL,custom_theme=theme_jh,title=NULL)


svg(filename="Git_Repo/code/results_abi/kmeans_by_cluster_plot_3.svg",width=10,height=10)
print(kmeans_mean_by_cluster)
dev.off()

##### Mean by cluster plot, k=4 #####
##### Doesn't work currently #####

kmeans_mean_by_cluster_4=mean_by_cluster(data=biobank_numeric, classes=as.factor(kmeans_optimal_4$cluster),
                                       color_scale=NULL,custom_theme=theme_jh,title=NULL)


svg(filename="Git_Repo/code/results_abi/kmeans_by_cluster_plot_4.svg",width=10,height=10)
print(kmeans_mean_by_cluster)
dev.off()

################################################
# Distributions of variables by cluster
################################################

##### Errors in this doesn't currently run #####
##### Function for distribution by cluster #####

library(gridExtra)
library(GetR)
library(shadowtext)

distribution_by_cluster=function(data,classes,layout=NULL,color_scale=NULL,custom_theme=NULL,title=NULL) {
  
  if (is.null(layout)) {
    layout=c(ceiling(sqrt(ncol(data))),ceiling(sqrt(ncol(data))))
  }
  
  plot_list=list()
  
  for (k in 1:ncol(data)) {
    
    
    density_list_raw=by(as.numeric(data[,k]),INDICES=as.numeric(as.character(classes)),density,simplify = TRUE)
    
    classes_vector=NULL
    classes_list=lapply(density_list_raw,function(x) {length(x[[1]])})
    
    for (i in 1:length(density_list_raw)) {
      classes_vector=c(classes_vector,rep(names(classes_list[i]), classes_list[i]))
    }
    
    tmp_plot_data=data.frame(x=unlist(lapply(density_list_raw,function(x) {x[[1]]})),
                             y=unlist(lapply(density_list_raw,function(x) {x[[2]]})),
                             classes=as.factor(classes_vector))
    
    tmp_plot=ggplot(data=tmp_plot_data) +
      geom_line(aes(x=x,y=y,color=classes),alpha=1) +
      xlab("") + ylab("") + custom_theme + ggtitle(colnames(data)[k]) + custom_theme
    if(!is.null(color_scale)) {
      tmp_plot = tmp_plot + scale_color_manual(values=distinct_scale)
    }
    
    plot_list[[k]]=tmp_plot
  }
  
  final_plot=grid.arrange(gTree(children = gList(rectGrob(gp=gpar(fill="white",col="white", lwd=0)),
                                                 arrangeGrob(grobs=plot_list, ncol=layout[1], nrow=layout[2])
  )))
  
  return(final_plot)
  
}

for (k in 1:length(biobank_numeric)) {
  
  distribution_by_cluster_plot=distribution_by_cluster(data=biobank_numeric,
                                                                        classes=as.factor(kmeans_optimal_2$cluster),layout=c(3,3),
                                                                        color_scale=NULL,custom_theme=theme_jh,
                                                                        title=paste0("Distributions of continuous variables by classes (",
                                                                                     k,"/",length(biobank_numeric),")"))
  
  svg(filename=paste0("Git_Repo/code/results_abi/kmeans_distribution_by_cluster_",k,"_",length(cont_variables_split),".svg"),
      width=10,height=10)
  grid.draw(distribution_by_cluster_plot)
  dev.off()
  
}



