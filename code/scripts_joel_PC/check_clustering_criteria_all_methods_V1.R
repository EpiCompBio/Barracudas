################################################################################
# LOADING LIBRARIES
################################################################################

using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using("magrittr","dplyr","FactoMineR")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


########################################################################################################
########################################################################################################
# V1 : Pretty raw data 
########################################################################################################
########################################################################################################

#FAMD_kmeans_ordinal_continuous
cluster_crit_df_FAMD_kmeans_ordinal_continuous_multi_morbid=
  readRDS(paste0("../real_results_from_HPC/results_joel_HPC/FAMD_kmeans_ordinal_continuous/2_clusters/",
                 "cluster_crit_df_FAMD_kmeans_ordinal_continuous_multi_morbid.rds"))

print(cluster_crit_df_FAMD_kmeans_ordinal_continuous_multi_morbid)


#FAMD_kmeans_ordinal_factors
cluster_crit_df_FAMD_kmeans_ordinal_factors_multi_morbid=
  readRDS(paste0("../real_results_from_HPC/results_joel_HPC/FAMD_kmeans_ordinal_factors/2_clusters/",
                 "cluster_crit_df_FAMD_kmeans_ordinal_factors_multi_morbid.rds"))

print(cluster_crit_df_FAMD_kmeans_ordinal_factors_multi_morbid)


#FAMD_GMM_ordinal_continuous
cluster_crit_df_FAMD_GMM_ordinal_continuous_multi_morbid=
  readRDS(paste0("../real_results_from_HPC/results_joel_HPC/FAMD_GMM_ordinal_continuous/",
                 "cluster_crit_df_FAMD_GMM_ordinal_continuous_multi_morbid.rds"))

print(cluster_crit_df_FAMD_GMM_ordinal_continuous_multi_morbid)


#FAMD_GMM_ordinal_factors
cluster_crit_df_FAMD_GMM_ordinal_factors_multi_morbid=
  readRDS(paste0("../real_results_from_HPC/results_joel_HPC/FAMD_GMM_ordinal_factors/",
                 "cluster_crit_df_FAMD_GMM_ordinal_factors_multi_morbid.rds"))

print(cluster_crit_df_FAMD_GMM_ordinal_factors_multi_morbid)

  
########################################################################################################
########################################################################################################
# V4 : Cleaned data
########################################################################################################
########################################################################################################
