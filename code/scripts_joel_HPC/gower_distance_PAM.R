################################################################################
# LOADING LIBRARIES
################################################################################

# using<-function(...) {
#  libs<-unlist(list(...))
#   req<-unlist(lapply(libs,require,character.only=TRUE))
#  need<-libs[req==FALSE]
#  if(length(need)>0){
#    install.packages(need)
#    lapply(need,require,character.only=TRUE)
#  }
# }
# 
# using("magrittr","cluster","dplyr")

library(cluster,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(magrittr,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(dplyr,lib.loc ="/home/jheller/anaconda3/lib/R/library")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

# file_path<-dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(file_path)

# setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


multi_morbid=readRDS("../data/processed/multi_morbid_ordinal_continuous.rds")
# multi_morbid=multi_morbid[1:200,]



sapply(multi_morbid,class)


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


FAMD_multi_morbid_res=readRDS("../data/processed/FAMD_ordinal_factors_multi_morbid_res.rds")

nb_comp_FAMD_multi_morbid=which(FAMD_multi_morbid_res$eig[,3] > 60)[1]


################################################################################
# Gower distance for the proximty measures
################################################################################

gower_dissimilarity_multi_morbid_res=readRDS("../data/processed/gower_dissimilarity_multi_morbid_res.rds")



################################################################################
# Partitioning around medoids on the randomForest proximity measure
################################################################################

gower_pam_multi_morbid=pam(gower_dissimilarity_multi_morbid_res, 2)


saveRDS(gower_pam_multi_morbid,"../results/results_joel_HPC/gower_pam/gower_pam_multi_morbid.rds")


clusters_gower_pam_multi_morbid=gower_pam_multi_morbid$clustering

gower_pam_multi_morbid_plot_d12=make_FAMD_ind_plot_classes(FAMD_multi_morbid_res,classes=clusters_gower_pam_multi_morbid,
                                                        dims=c(1,2),
                                                        custom_theme=theme_jh,color_scale=distinct_scale)



gower_pam_multi_morbid_plot_d34=make_FAMD_ind_plot_classes(FAMD_multi_morbid_res,classes=clusters_gower_pam_multi_morbid,
                                                        dims=c(3,4),
                                                        custom_theme=theme_jh,color_scale=distinct_scale)


svg(filename="../results/results_joel_HPC/gower_pam/gower_pam_multi_morbid_plot_d12.svg",width=10,height=10)
print(gower_pam_multi_morbid_plot_d12)
dev.off()

svg(filename="../results/results_joel_HPC/gower_pam/gower_pam_multi_morbid_plot_d34.svg",width=10,height=10)
print(gower_pam_multi_morbid_plot_d34)
dev.off()

################################################
# Means continuous variables by cluster
################################################

cat_variables=colnames(multi_morbid)[sapply(sapply(multi_morbid,class),function(x) {x[[1]]}) == "factor" |
                                       sapply(sapply(multi_morbid,class),function(x) {x[[1]]}) == "ordered"]
cont_variables=colnames(multi_morbid)[sapply(multi_morbid,class) == "numeric"]




# cat_variables=colnames(multi_morbid)[sapply(multi_morbid,class) == "factor" | sapply(multi_morbid,class) != "ordered factor"]
# cont_variables=colnames(multi_morbid)[sapply(sapply(multi_morbid,class),function(x) {x[[1]]}) != "factor" &
#                                          sapply(sapply(multi_morbid,class),function(x) {x[[1]]}) != "ordered"]
# cont_variables=cont_variables[2:length(cont_variables)]

gower_pam_mean_by_cluster_continuous_plot=mean_by_cluster_continuous(data=multi_morbid[,cont_variables],
                                                                  classes=as.factor(clusters_gower_pam_multi_morbid),
                                                                  color_scale=NULL,custom_theme=theme_jh,title=NULL)


svg(filename="../results/results_joel_HPC/gower_pam/gower_pam_multi_morbid_mean_by_cluster_continuous_plot.svg",width=10,height=10)
print(gower_pam_mean_by_cluster_continuous_plot)
dev.off()


################################################
# Distributions Cat variables by cluster
################################################



cat_variables_split=splitIndices(nx=length(cat_variables), ncl=ceiling(length(cat_variables) / 9))

for (k in 1:length(cat_variables_split)) {
  
  
  gower_pam_cat_distribution_by_cluster=cat_distribution_by_cluster(data=multi_morbid[,cat_variables[cat_variables_split[[k]]]],
                                                                 classes=as.factor(clusters_gower_pam_multi_morbid),layout=c(3,3),
                                                                 color_scale=NULL,custom_theme=theme_jh,
                                                                 title=paste0("Distributions of categorical variables by classes (",
                                                                              k,"/",length(cat_variables_split),")"))
  
  
  svg(filename=paste0("../results/results_joel_HPC/gower_pam/gower_pam_multi_morbid_cat_distribution_by_cluster_",k,"_",length(cat_variables_split),".svg"),
      width=10,height=10)
  grid.draw(gower_pam_cat_distribution_by_cluster)
  dev.off()
  
}

################################################
# Distributions Cont variables by cluster
################################################

cont_variables_split=splitIndices(nx=length(cont_variables), ncl=ceiling(length(cont_variables) / 9))


for (k in 1:length(cont_variables_split)) {
  
  gower_pam_cont_distribution_by_cluster=cont_distribution_by_cluster(data=multi_morbid[,cont_variables[cont_variables_split[[k]]]],
                                                                   classes=as.factor(clusters_gower_pam_multi_morbid),layout=c(3,3),
                                                                   color_scale=NULL,custom_theme=theme_jh,
                                                                   title=paste0("Distributions of continuous variables by classes (",
                                                                                k,"/",length(cont_variables_split),")"))
  
  svg(filename=paste0("../results/results_joel_HPC/gower_pam/gower_pam_multi_morbid_cont_distribution_by_cluster_",k,"_",length(cont_variables_split),".svg"),
      width=10,height=10)
  grid.draw(gower_pam_cont_distribution_by_cluster)
  dev.off()
  
}