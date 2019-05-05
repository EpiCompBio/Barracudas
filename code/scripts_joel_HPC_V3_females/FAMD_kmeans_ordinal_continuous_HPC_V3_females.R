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
# using("FactoMineR","ggplot2","ggrepel","viridis","RColorBrewer","reshape2","magrittr",
#       "gridExtra","grid","dplyr","parallel","clusterCrit","randomForest","doParallel")



#Package from sourcing functions
library(FactoMineR,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(ggplot2,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(ggrepel,lib.loc ="/home/jheller/anaconda3/lib/R/library")


library(viridis,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(RColorBrewer, lib.loc ="/home/jheller/anaconda3/lib/R/library")


library(reshape2,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(magrittr,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(gridExtra,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(grid,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(dplyr,lib.loc ="/home/jheller/anaconda3/lib/R/library")


# Other packages used in the script
library(parallel,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(clusterCrit,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(randomForest,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(doParallel,lib.loc ="/home/jheller/anaconda3/lib/R/library")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

# setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


multi_morbid=readRDS("../data/processed_V3_females/multi_morbid_ordinal_continuous_HW_mod_female.rds")
# multi_morbid=multi_morbid[1:200,]

source("../data/processed_V3_females/var_groupings_V3.R")
source("code/utility_functions/FAMD_plots_utility.R")
source("code/utility_functions/colors_themes_utility.R")
source("code/utility_functions/clustering_utility.R")


################################################################################
# Parameters for inside the script
################################################################################
n_Cores=20

do_choose_nclusters=FALSE
do_rep_clustering=TRUE
do_stability=TRUE

n_rep_choose_nb_clust=10
seed_start_choose_clust=200

n_rep_clustering=50
seed_start_clustering=1000

n_sub_sample=100
int_val=90
add_to_seed_subsampling=0


#FOR LOCAL

# n_Cores=7
# 
# do_choose_nclusters=TRUE
# do_rep_clustering=FALSE
# do_stability=FALSE
# 
# n_rep_choose_nb_clust=5
# seed_start_choose_clust=200
# 
# n_rep_clustering=10
# seed_start_clustering=1000
# 
# n_sub_sample=50
# int_val=90
# add_to_seed_subsampling=0

################################################################################
################################################################################
# multi-morbid individuals only
################################################################################
################################################################################

FAMD_multi_morbid_res=readRDS("../data/processed_V3_females/FAMD_ordinal_continuous_multi_morbid_res.rds")


nb_comp_FAMD_multi_morbid=which(FAMD_multi_morbid_res$eig[,3] > 90)[1]

################################################################################
# Choosing the number of clusters for kmeans
################################################################################

if (do_choose_nclusters==TRUE) {
  
  n_classes=2:5
  
  cluster_crit_df=as.data.frame(matrix(0,nrow=length(n_classes),ncol=4))
  cluster_crit_df[,1]=n_classes
  colnames(cluster_crit_df)=c("n_classes","Cal_Har","Silhouette","Point_Bi")
  
  
  
  
  
  # Different numbers of centers
  for (k in 1:length(n_classes)) {
    
    
    registerDoParallel(n_Cores)
    
    cluster_criterion_list=foreach(i=1:n_rep_choose_nb_clust,
                                   .packages = c('FactoMineR',"randomForest","clusterCrit")) %dopar% {
                                     
                                     set.seed(seed_start_choose_clust+i)
                                     FAMD_kmeans_multi_morbid=kmeans(FAMD_multi_morbid_res$ind$coord[,1:nb_comp_FAMD_multi_morbid],centers=n_classes[k])
                                     
                                     criteria_vector=unlist(intCriteria(traj=as.matrix(FAMD_multi_morbid_res$ind$coord[,1:nb_comp_FAMD_multi_morbid]),
                                                                        part=FAMD_kmeans_multi_morbid$cluster,c("Calinski_Harabasz","Silhouette","Point_Biserial")))
                                     
                                     return(criteria_vector)
                                   }
    
    stopImplicitCluster()
    
    
    cluster_crit_df[k,2]=max(sapply(cluster_criterion_list,function(x) {x[[1]]}))
    cluster_crit_df[k,3]=max(sapply(cluster_criterion_list,function(x) {x[[2]]}))
    cluster_crit_df[k,4]=min(sapply(cluster_criterion_list,function(x) {x[[3]]}))
    
    
    if (!is.finite(cluster_crit_df[k,2])) {
      cluster_crit_df[k,2]=0
    } 
    
    if (!is.finite(cluster_crit_df[k,3])) {
      cluster_crit_df[k,3]=0
    } 
    
    if (!is.finite(cluster_crit_df[k,4])) {
      cluster_crit_df[k,4]=0
    } 
    
  }
  
  saveRDS(cluster_crit_df,"../results/results_joel_HPC_V3_female/FAMD_kmeans_ordinal_continuous/cluster_crit_df_FAMD_kmeans_ordinal_continuous_multi_morbid.rds")
  
}



################################################################################
# Kmeans on the FAMD row coordinates with the best number of clusters
################################################################################

if (do_rep_clustering==TRUE) {
  
  cluster_crit_vector=rep(0,n_rep_clustering)
  
  registerDoParallel(n_Cores)
  
  cluster_crit_vector=unlist(foreach(k=1:n_rep_clustering,
                                     .packages = c('FactoMineR',"randomForest","clusterCrit")) %dopar% {
                                       
                                       
                                       set.seed(seed_start_clustering+k)
                                       FAMD_kmeans_multi_morbid=kmeans(FAMD_multi_morbid_res$ind$coord[,1:nb_comp_FAMD_multi_morbid],centers=2)
                                       cluster_crit=unlist(intCriteria(traj=as.matrix(FAMD_multi_morbid_res$ind$coord[,1:nb_comp_FAMD_multi_morbid]),
                                                                       part=FAMD_kmeans_multi_morbid$cluster,c("Calinski_Harabasz")))
                                       return(cluster_crit)
                                       
                                     })
  stopImplicitCluster()
  
  set.seed(which.max(cluster_crit_vector) + seed_start_clustering)
  FAMD_kmeans_multi_morbid=kmeans(FAMD_multi_morbid_res$ind$coord[,1:nb_comp_FAMD_multi_morbid],centers=2)
  
} else {
  FAMD_kmeans_multi_morbid=kmeans(FAMD_multi_morbid_res$ind$coord[,1:nb_comp_FAMD_multi_morbid],centers=2)
}



saveRDS(FAMD_kmeans_multi_morbid,"../results/results_joel_HPC_V3_female/FAMD_kmeans_ordinal_continuous/FAMD_kmeans_ordinal_continuous_multi_morbid.rds")


clusters_FAMD_kmeans_multi_morbid=FAMD_kmeans_multi_morbid$cluster

FAMD_kmeans_multi_morbid_plot_d12=make_FAMD_ind_plot_classes(FAMD_multi_morbid_res,classes=clusters_FAMD_kmeans_multi_morbid,
                                                             dims=c(1,2),
                                                             custom_theme=theme_jh,color_scale=distinct_scale,show_labels = FALSE)



FAMD_kmeans_multi_morbid_plot_d34=make_FAMD_ind_plot_classes(FAMD_multi_morbid_res,classes=clusters_FAMD_kmeans_multi_morbid,
                                                             dims=c(3,4),
                                                             custom_theme=theme_jh,color_scale=distinct_scale,show_labels = FALSE)


svg(filename="../results/results_joel_HPC_V3_female/FAMD_kmeans_ordinal_continuous/FAMD_kmeans_ordinal_continuous_multi_morbid_plot_d12.svg",width=10,height=10)
print(FAMD_kmeans_multi_morbid_plot_d12)
dev.off()

svg(filename="../results/results_joel_HPC_V3_female/FAMD_kmeans_ordinal_continuous/FAMD_kmeans_ordinal_continuous_multi_morbid_plot_d34.svg",width=10,height=10)
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


svg(filename="../results/results_joel_HPC_V3_female/FAMD_kmeans_ordinal_continuous/FAMD_kmeans_ordinal_continuous_multi_morbid_mean_by_cluster_continuous_plot.svg",width=10,height=10)
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
  
  
  svg(filename=paste0("../results/results_joel_HPC_V3_female/FAMD_kmeans_ordinal_continuous/FAMD_kmeans_ordinal_continuous_multi_morbid_cat_distribution_by_cluster_",k,"_",length(cat_variables_split),".svg"),
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
  
  svg(filename=paste0("../results/results_joel_HPC_V3_female/FAMD_kmeans_ordinal_continuous/FAMD_kmeans_ordinal_continuous_multi_morbid_cont_distribution_by_cluster_",k,"_",length(cont_variables_split),".svg"),
      width=10,height=10)
  grid.draw(FAMD_kmeans_cont_distribution_by_cluster)
  dev.off()
  
}


################################################
# Define groupings
################################################

grouping_names=list(Disease=Disease,Demographics=Demographics,BMI_related=BMI_related,
                    Activity=Activity,Vital_signs=Vital_signs,Tobacco=Tobacco,
                    Alcohol=Alcohol,Dietary=Dietary,Med_surg_hx=Med_surg_hx)


################################################
# Distribution tests
################################################


distribution_test_df=data.frame(matrix(0,ncol=3,nrow=length(c(cont_variables,cat_variables))))
colnames(distribution_test_df)=c("var_name","Type","p_value")


distribution_test_df[,1]=c(cont_variables,cat_variables)
distribution_test_df[,2]=c(rep("Cont",length(cont_variables)),rep("Cat",length(cat_variables)))


for (k in 1:nrow(distribution_test_df)) {
  
  if (distribution_test_df[k,2]=="Cont") {
    
    anova_res=summary(lm(outcome ~ clusters,
                         data=data.frame(outcome=multi_morbid[,distribution_test_df[k,1]],clusters=as.factor(clusters_FAMD_kmeans_multi_morbid))))
    distribution_test_df[k,3]=df(anova_res$fstatistic[1], anova_res$fstatistic[2], anova_res$fstatistic[3])
    
  } else if (distribution_test_df[k,2]=="Cat") {
    
    distribution_test_df[k,3]=chisq.test(multi_morbid[,distribution_test_df[k,1]],as.factor(clusters_FAMD_kmeans_multi_morbid))$p.value
    
  }
  
}

distribution_test_df[,3]=p.adjust(distribution_test_df[,3],method="bonferroni")



distribution_test_df=distribution_test_df[match(colnames(multi_morbid)[2:ncol(multi_morbid)],distribution_test_df[,1]),]


significant_cluster_differences_by_variable_plot=make_significant_cluster_differences_by_variable_plot(distribution_test_df,
                                                                                                       grouping_names=grouping_names,
                                                                                                       color_scale=NULL,custom_theme=theme_jh,
                                                                                                       threshold=10^-50)


svg(filename=paste0("../results/results_joel_HPC_V3_female/FAMD_kmeans_ordinal_continuous/",
                    "FAMD_kmeans_ordinal_continuous_multi_morbid_cluster_differences_by_variable.svg"),
    width=10,height=10)
print(significant_cluster_differences_by_variable_plot)
dev.off()


################################################
# random Forest variable importance
################################################


randomForest_multi_morbid=randomForest(multi_morbid[,2:ncol(multi_morbid)], y=as.factor(clusters_FAMD_kmeans_multi_morbid),ntree=500)

var_importance_df=data.frame(matrix(0,ncol=2,nrow=length(c(cont_variables,cat_variables))))
colnames(var_importance_df)=c("var_name","Type")

var_importance_df[,1]=c(cont_variables,cat_variables)
var_importance_df[,2]=c(rep("Cont",length(cont_variables)),rep("Cat",length(cat_variables)))         



var_importance_df=var_importance_df[match(colnames(multi_morbid)[2:ncol(multi_morbid)],var_importance_df[,1]),]
var_importance_df$var_importance=randomForest_multi_morbid$importance


variable_importance_plot=make_variable_importance_plot(var_importance_df,grouping_names=grouping_names, color_scale=NULL,custom_theme=theme_jh,
                                                       threshold=50)


svg(filename=paste0("../results/results_joel_HPC_V3_female/FAMD_kmeans_ordinal_continuous/",
                    "FAMD_kmeans_ordinal_continuous_multi_morbid_variable_importance.svg"),
    width=10,height=10)
print(variable_importance_plot)
dev.off()



################################################################################################
# Cluster stability
################################################################################################

if (do_stability==TRUE) {
  
  registerDoParallel(n_Cores)
  
  
  lower_bound_int=floor(n_sub_sample * (100 - int_val) / 200) + 1
  upper_bound_int=floor(n_sub_sample * (1 - (100 - int_val) / 200))+  1
  
  # matrix(0,ncol=ncol(multi_morbid),nrow=n_sub_sample)
  
  
  
  var_importance_stab_list=foreach(i=1:n_sub_sample,
                                   .packages = c('FactoMineR',"randomForest","clusterCrit")) %dopar% {
                                     
                                     
                                     set.seed(i)
                                     multi_morbid_subsample=multi_morbid[sample(1:nrow(multi_morbid),size=floor(nrow(multi_morbid)*0.8)),]
                                     
                                     
                                     FAMD_multi_morbid_subsample_res=FAMD(multi_morbid_subsample[,15:ncol(multi_morbid_subsample)],
                                                                          ncp=ncol(multi_morbid_subsample) +20, graph = FALSE)
                                     
                                     
                                     
                                     
                                     nb_comp_FAMD_multi_morbid_subsample=which(FAMD_multi_morbid_subsample_res$eig[,3] > 90)[1]
                                     
                                     
                                     cluster_crit_vector=rep(0,10)
                                     add_to_seed_subsampling=0
                                     for (k in 1:10) {
                                       set.seed(k+add_to_seed_subsampling)
                                       
                                       FAMD_kmeans_multi_morbid_subsample=
                                         kmeans(FAMD_multi_morbid_subsample_res$ind$coord[,1:nb_comp_FAMD_multi_morbid_subsample],centers=2)
                                       cluster_crit_vector[k]=
                                         unlist(intCriteria(traj=as.matrix(FAMD_multi_morbid_subsample_res$ind$coord[,1:nb_comp_FAMD_multi_morbid_subsample]),
                                                            part=FAMD_kmeans_multi_morbid_subsample$cluster,c("Calinski_Harabasz")))
                                       
                                     }
                                     set.seed(which.max(cluster_crit_vector+add_to_seed_subsampling))
                                     FAMD_kmeans_multi_morbid_subsample=
                                       kmeans(FAMD_multi_morbid_subsample_res$ind$coord[,1:nb_comp_FAMD_multi_morbid_subsample],centers=2)
                                     
                                     clusters_FAMD_kmeans_multi_morbid_subsample=FAMD_kmeans_multi_morbid_subsample$cluster
                                     
                                     
                                     
                                     randomForest_multi_morbid_subsample=
                                       randomForest(multi_morbid_subsample[,2:ncol(multi_morbid_subsample)],
                                                    y=as.factor(clusters_FAMD_kmeans_multi_morbid_subsample),ntree=500)
                                     
                                     return(randomForest_multi_morbid_subsample$importance)
                                     
                                   }
  
  
  stopImplicitCluster()
  
  var_importance_stab_matrix=as.data.frame(matrix(unlist(var_importance_stab_list),
                                                  byrow=TRUE,nrow=length(var_importance_stab_list),
                                                  dimnames = list(NULL,
                                                                  rownames(var_importance_stab_list[[1]]))))
  
  var_importance_stab_matrix=apply(var_importance_stab_matrix,2,sort)
  
  
  var_importance_stab_df=data.frame(matrix(0,ncol=2,nrow=length(c(cont_variables,cat_variables))))
  colnames(var_importance_stab_df)=c("var_name","Type")
  
  var_importance_stab_df[,1]=c(cont_variables,cat_variables)
  var_importance_stab_df[,2]=c(rep("Cont",length(cont_variables)),rep("Cat",length(cat_variables)))         
  var_importance_stab_df=var_importance_stab_df[match(colnames(multi_morbid[,2:ncol(multi_morbid)]),var_importance_stab_df[,1]),]
  
  
  var_importance_stab_df$median=apply(var_importance_stab_matrix,2,median)
  var_importance_stab_df$LB=apply(var_importance_stab_matrix,2,function(x) {x[lower_bound_int]})
  var_importance_stab_df$UB=apply(var_importance_stab_matrix,2,function(x) {x[upper_bound_int]})
  
  
  
  variable_importance_stability_plot=make_variable_importance_stability_plot(var_importance_stab_df,grouping_names=grouping_names, color_scale=NULL,custom_theme=theme_jh,
                                                                             threshold=50)
  
  
  svg(filename=paste0("../results/results_joel_HPC_V3_female/FAMD_kmeans_ordinal_continuous/",
                      "FAMD_kmeans_ordinal_continuous_multi_morbid_variable_importance_stability.svg"),
      width=10,height=10)
  print(variable_importance_stability_plot)
  dev.off()
  
}
