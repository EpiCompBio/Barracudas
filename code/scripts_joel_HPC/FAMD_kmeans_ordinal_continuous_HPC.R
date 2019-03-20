################################################################################
# LOADING LIBRARIES
################################################################################

# using<-function(...) {
#   libs<-unlist(list(...))
#   req<-unlist(lapply(libs,require,character.only=TRUE))
#   need<-libs[req==FALSE]
#   if(length(need)>0){
#     install.packages(need)
#     lapply(need,require,character.only=TRUE)
#   }
# }
# 
# using("FactoMineR","parallel","clusterCrit","reshape2","magrittr","gridExtra","grid","dplyr","parallel","clusterCrit")

library(FactoMineR,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(reshape2,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(magrittr,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(gridExtra,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(grid,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(dplyr,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(parallel,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(clusterCrit,lib.loc ="/home/jheller/anaconda3/lib/R/library")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

# file_path<-dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(file_path)

# setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


multi_morbid=readRDS("../data/processed/multi_morbid_ordinal_continuous.rds")
# multi_morbid=multi_morbid[1:200,]


# source("C:/Users/JOE/Documents/R_utility_and_self_implementations/FAMD_plots_utility.R")
# source("C:/Users/JOE/Documents/R_utility_and_self_implementations/colors_themes_utility.R")
# source("C:/Users/JOE/Documents/R_utility_and_self_implementations/clustering_utility.R")



source("code/utility_functions/FAMD_plots_utility.R")
source("code/utility_functions/colors_themes_utility.R")
source("code/utility_functions/clustering_utility.R")


################################################################################
################################################################################
# multi-morbid individuals only
################################################################################
################################################################################

FAMD_multi_morbid_res=readRDS("../data/processed/FAMD_ordinal_continuous_multi_morbid_res.rds")

nb_comp_FAMD_multi_morbid=which(FAMD_multi_morbid_res$eig[,3] > 80)[1]


################################################################################
# Choosing the number of clusters for kmeans
################################################################################


n_classes=2:8

cluster_crit_df=as.data.frame(matrix(0,nrow=length(n_classes),ncol=3))
cluster_crit_df[,1]=n_classes
colnames(cluster_crit_df)=c("n_classes","Cal_Har","Silhouette")


# Different numbers of centers
for (k in 1:length(n_classes)) {
  
  FAMD_kmeans_multi_morbid=kmeans(FAMD_multi_morbid_res$ind$coord[,1:nb_comp_FAMD_multi_morbid],centers=n_classes[k])
  
  cluster_crit_df[k,2:3]=unlist(intCriteria(traj=as.matrix(FAMD_multi_morbid_res$ind$coord[,1:nb_comp_FAMD_multi_morbid]),
                                            part=FAMD_kmeans_multi_morbid$cluster,c("Calinski_Harabasz","Silhouette")))
}


saveRDS(cluster_crit_df,"../results/results_joel_HPC/FAMD_kmeans_ordinal_continuous/cluster_crit_df_FAMD_kmeans_ordinal_continuous_multi_morbid.rds")

################################################################################
# Kmeans on the FAMD row coordinates with the best number of clusters
################################################################################

FAMD_kmeans_multi_morbid=kmeans(FAMD_multi_morbid_res$ind$coord[,1:nb_comp_FAMD_multi_morbid],centers=3)
# FAMD_kmeans_multi_morbid=readRDS("../results/results_joel_HPC/FAMD_kmeans/FAMD_kmeans_multi_morbid.rds")


saveRDS(FAMD_kmeans_multi_morbid,"../results/results_joel_HPC/FAMD_kmeans_ordinal_continuous/FAMD_kmeans_ordinal_continuous_multi_morbid.rds")


clusters_FAMD_kmeans_multi_morbid=FAMD_kmeans_multi_morbid$cluster

FAMD_kmeans_multi_morbid_plot_d12=make_FAMD_ind_plot_classes(FAMD_multi_morbid_res,classes=clusters_FAMD_kmeans_multi_morbid,
                                                             dims=c(1,2),
                                                             custom_theme=theme_jh,color_scale=distinct_scale,show_labels = FALSE)



FAMD_kmeans_multi_morbid_plot_d34=make_FAMD_ind_plot_classes(FAMD_multi_morbid_res,classes=clusters_FAMD_kmeans_multi_morbid,
                                                             dims=c(3,4),
                                                             custom_theme=theme_jh,color_scale=distinct_scale,show_labels = FALSE)


svg(filename="../results/results_joel_HPC/FAMD_kmeans_ordinal_continuous/FAMD_kmeans_ordinal_continuous_multi_morbid_plot_d12.svg",width=10,height=10)
print(FAMD_kmeans_multi_morbid_plot_d12)
dev.off()

svg(filename="../results/results_joel_HPC/FAMD_kmeans_ordinal_continuous/FAMD_kmeans_ordinal_continuous_multi_morbid_plot_d34.svg",width=10,height=10)
print(FAMD_kmeans_multi_morbid_plot_d34)
dev.off()


################################################
# Means continuous variables by cluster
################################################

cat_variables=colnames(multi_morbid)[sapply(multi_morbid,class) == "factor"]
cont_variables=colnames(multi_morbid)[sapply(multi_morbid,class) != "factor"]
cont_variables=cont_variables[2:length(cont_variables)]


FAMD_kmeans_mean_by_cluster_continuous_plot=mean_by_cluster_continuous(data=multi_morbid[,cont_variables],
                                                                       classes=as.factor(clusters_FAMD_kmeans_multi_morbid),
                                                                       color_scale=NULL,custom_theme=theme_jh,title=NULL)


svg(filename="../results/results_joel_HPC/FAMD_kmeans_ordinal_continuous/FAMD_kmeans_ordinal_continuous_multi_morbid_mean_by_cluster_continuous_plot.svg",width=10,height=10)
print(FAMD_kmeans_mean_by_cluster_continuous_plot)
dev.off()


################################################
# Distributions Cat variables by cluster
################################################



cat_variables_split=splitIndices(nx=length(cat_variables), ncl=ceiling(length(cat_variables) / 9))

for (k in 1:length(cat_variables_split)) {
  
  
  FAMD_kmeans_cat_distribution_by_cluster=cat_distribution_by_cluster(data=multi_morbid[,cat_variables[cat_variables_split[[k]]]],
                                                                      classes=as.factor(clusters_FAMD_kmeans_multi_morbid),layout=c(3,3),
                                                                      color_scale=NULL,custom_theme=theme_jh,
                                                                      title=paste0("Distributions of categorical variables by classes (",
                                                                                   k,"/",length(cat_variables_split),")"))
  
  
  svg(filename=paste0("../results/results_joel_HPC/FAMD_kmeans_ordinal_continuous/FAMD_kmeans_ordinal_continuous_multi_morbid_cat_distribution_by_cluster_",k,"_",length(cat_variables_split),".svg"),
      width=10,height=10)
  grid.draw(FAMD_kmeans_cat_distribution_by_cluster)
  dev.off()
  
}

################################################
# Distributions Cont variables by cluster
################################################

cont_variables_split=splitIndices(nx=length(cont_variables), ncl=ceiling(length(cont_variables) / 9))


for (k in 1:length(cont_variables_split)) {
  
  FAMD_kmeans_cont_distribution_by_cluster=cont_distribution_by_cluster(data=multi_morbid[,cont_variables[cont_variables_split[[k]]]],
                                                                        classes=as.factor(clusters_FAMD_kmeans_multi_morbid),layout=c(3,3),
                                                                        color_scale=NULL,custom_theme=theme_jh,
                                                                        title=paste0("Distributions of continuous variables by classes (",
                                                                                     k,"/",length(cont_variables_split),")"))
  
  svg(filename=paste0("../results/results_joel_HPC/FAMD_kmeans_ordinal_continuous/FAMD_kmeans_ordinal_continuous_multi_morbid_cont_distribution_by_cluster_",k,"_",length(cont_variables_split),".svg"),
      width=10,height=10)
  grid.draw(FAMD_kmeans_cont_distribution_by_cluster)
  dev.off()
  
}

