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
#       "gridExtra","grid","dplyr","parallel","apcluster","randomForest")



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



#Package needed for the clustering method
library(apcsluter,lib.loc ="/home/jheller/anaconda3/lib/R/library")



# Other packages used in the script
library(parallel,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(randomForest,lib.loc ="/home/jheller/anaconda3/lib/R/library")




################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

# file_path<-dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(file_path)

# setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


multi_morbid=readRDS("../data/processed/multi_morbid_ordinal_keep.rds")
# multi_morbid=multi_morbid[1:200,]


# source("C:/Users/JOE/Documents/R_utility_and_self_implementations/FAMD_plots_utility.R")
# source("C:/Users/JOE/Documents/R_utility_and_self_implementations/colors_themes_utility.R")
# source("C:/Users/JOE/Documents/R_utility_and_self_implementations/clustering_utility.R")


source("../data/processed/var_groupings.R")
source("code/utility_functions/FAMD_plots_utility.R")
source("code/utility_functions/colors_themes_utility.R")
source("code/utility_functions/clustering_utility.R")


################################################################################
################################################################################
# multi-morbid individuals only
################################################################################
################################################################################

FAMD_multi_morbid_res=readRDS("../data/processed/FAMD_ordinal_factors_multi_morbid_res.rds")

nb_comp_FAMD_multi_morbid=which(FAMD_multi_morbid_res$eig[,3] > 90)[1]



################################################################################
# randomForest proximity matrix multi-morbid individuals
################################################################################

RF_proximity_measure_multi_morbid_res=readRDS("../data/processed/RF_proximity_measure_ordinal_factors_multi_morbid_res.rds")


################################################################################
# Partitioning around medoids on the randomForest proximity measure
################################################################################

RF_aff_prop_multi_morbid=apclusterK(RF_proximity_measure_multi_morbid_res, K=2)

clusters_RF_aff_prop_multi_morbid=rep(0,nrow(RF_proximity_measure_multi_morbid_res))

for (k in 1:length(RF_aff_prop_multi_morbid@clusters)) {
  for (l in 1:length(RF_aff_prop_multi_morbid@clusters[[k]])) {
    clusters_RF_aff_prop_multi_morbid[RF_aff_prop_multi_morbid@clusters[[k]][l]]=k
  }
}

saveRDS(RF_aff_prop_multi_morbid,"../results/results_joel_HPC/RF_aff_prop_ordinal_factors/RF_aff_prop_multi_morbid.rds")


RF_aff_prop_multi_morbid_plot_d12=make_FAMD_ind_plot_classes(FAMD_multi_morbid_res,classes=clusters_RF_aff_prop_multi_morbid,
                                                             dims=c(1,2),
                                                             custom_theme=theme_jh,color_scale=distinct_scale,show_labels = FALSE)



RF_aff_prop_multi_morbid_plot_d34=make_FAMD_ind_plot_classes(FAMD_multi_morbid_res,classes=clusters_RF_aff_prop_multi_morbid,
                                                             dims=c(3,4),
                                                             custom_theme=theme_jh,color_scale=distinct_scale,show_labels = FALSE)


svg(filename="../results/results_joel_HPC/RF_aff_prop_ordinal_factors/RF_aff_prop_ordinal_factors_multi_morbid_plot_d12.svg",width=10,height=10)
print(RF_aff_prop_multi_morbid_plot_d12)
dev.off()

svg(filename="../results/results_joel_HPC/RF_aff_prop_ordinal_factors/RF_aff_prop_ordinal_factors_multi_morbid_plot_d34.svg",width=10,height=10)
print(RF_aff_prop_multi_morbid_plot_d34)
dev.off()


################################################
# Means continuous variables by cluster
################################################

cat_variables=colnames(multi_morbid)[sapply(sapply(multi_morbid,class),function(x) {x[[1]]}) == "factor" |
                                       sapply(sapply(multi_morbid,class),function(x) {x[[1]]}) == "ordered"]
cont_variables=colnames(multi_morbid)[sapply(multi_morbid,class) == "numeric"]


RF_aff_prop_mean_by_cluster_continuous_plot=mean_by_cluster_continuous(data=multi_morbid[,cont_variables],
                                                                       classes=as.factor(clusters_RF_aff_prop_multi_morbid),
                                                                       color_scale=NULL,custom_theme=theme_jh,title=NULL)


svg(filename="../results/results_joel_HPC/RF_aff_prop_ordinal_factors/RF_aff_prop_ordinal_factors_multi_morbid_mean_by_cluster_continuous_plot.svg",width=10,height=10)
print(RF_aff_prop_mean_by_cluster_continuous_plot)
dev.off()


################################################
# Distributions Cat variables by cluster
################################################



cat_variables_split=splitIndices(nx=length(cat_variables), ncl=ceiling(length(cat_variables) / 9))

for (k in 1:length(cat_variables_split)) {
  
  
  RF_aff_prop_cat_distribution_by_cluster=cat_distribution_by_cluster(data=multi_morbid[,cat_variables[cat_variables_split[[k]]]],
                                                                      classes=as.factor(clusters_RF_aff_prop_multi_morbid),layout=c(3,3),
                                                                      color_scale=NULL,custom_theme=theme_jh,
                                                                      title=paste0("Distributions of categorical variables by classes (",
                                                                                   k,"/",length(cat_variables_split),")"))
  
  
  svg(filename=paste0("../results/results_joel_HPC/RF_aff_prop_ordinal_factors/RF_aff_prop_ordinal_factors_multi_morbid_cat_distribution_by_cluster_",k,"_",length(cat_variables_split),".svg"),
      width=10,height=10)
  grid.draw(RF_aff_prop_cat_distribution_by_cluster)
  dev.off()
  
}

################################################
# Distributions Cont variables by cluster
################################################

cont_variables_split=splitIndices(nx=length(cont_variables), ncl=ceiling(length(cont_variables) / 9))


for (k in 1:length(cont_variables_split)) {
  
  RF_aff_prop_cont_distribution_by_cluster=cont_distribution_by_cluster(data=multi_morbid[,cont_variables[cont_variables_split[[k]]]],
                                                                        classes=as.factor(clusters_RF_aff_prop_multi_morbid),layout=c(3,3),
                                                                        color_scale=NULL,custom_theme=theme_jh,
                                                                        title=paste0("Distributions of continuous variables by classes (",
                                                                                     k,"/",length(cont_variables_split),")"))
  
  svg(filename=paste0("../results/results_joel_HPC/RF_aff_prop_ordinal_factors/RF_aff_prop_ordinal_factors_multi_morbid_cont_distribution_by_cluster_",k,"_",length(cont_variables_split),".svg"),
      width=10,height=10)
  grid.draw(RF_aff_prop_cont_distribution_by_cluster)
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
                         data=data.frame(outcome=multi_morbid[,distribution_test_df[k,1]],clusters=as.factor(clusters_RF_aff_prop_multi_morbid))))
    distribution_test_df[k,3]=df(anova_res$fstatistic[1], anova_res$fstatistic[2], anova_res$fstatistic[3])
    
  } else if (distribution_test_df[k,2]=="Cat") {
    
    distribution_test_df[k,3]=chisq.test(multi_morbid[,distribution_test_df[k,1]],as.factor(clusters_RF_aff_prop_multi_morbid))$p.value
    
  }
  
}

distribution_test_df[,3]=p.adjust(distribution_test_df[,3],method="bonferroni")



distribution_test_df=distribution_test_df[match(colnames(multi_morbid)[2:ncol(multi_morbid)],distribution_test_df[,1]),]


significant_cluster_differences_by_variable_plot=make_significant_cluster_differences_by_variable_plot(distribution_test_df,
                                                                                                       grouping_names=grouping_names,
                                                                                                       color_scale=NULL,custom_theme=theme_jh,
                                                                                                       threshold=10^-50)


svg(filename=paste0("../results/results_joel_HPC/Rf_aff_prop_ordinal_factors/",
                    "Rf_aff_prop_ordinal_factors_multi_morbid_cluster_differences_by_variable.svg"),
    width=10,height=10)
print(significant_cluster_differences_by_variable_plot)
dev.off()


################################################
# random Forest variable importance
################################################


randomForest_multi_morbid=randomForest(multi_morbid[,2:ncol(multi_morbid)], y=as.factor(clusters_RF_aff_prop_multi_morbid),ntree=500)

var_importance_df=data.frame(matrix(0,ncol=2,nrow=length(c(cont_variables,cat_variables))))
colnames(var_importance_df)=c("var_name","Type")

var_importance_df[,1]=c(cont_variables,cat_variables)
var_importance_df[,2]=c(rep("Cont",length(cont_variables)),rep("Cat",length(cat_variables)))         



var_importance_df=var_importance_df[match(colnames(multi_morbid)[2:ncol(multi_morbid)],var_importance_df[,1]),]
var_importance_df$var_importance=randomForest_multi_morbid$importance


variable_importance_plot=make_variable_importance_plot(var_importance_df,grouping_names=grouping_names, color_scale=NULL,custom_theme=theme_jh,
                                                       threshold=50)


svg(filename=paste0("../results/results_joel_HPC/Rf_aff_prop_ordinal_factors/",
                    "Rf_aff_prop_ordinal_factors_multi_morbid_variable_importance.svg"),
    width=10,height=10)
print(variable_importance_plot)
dev.off()
