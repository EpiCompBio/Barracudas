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

using("ggplot2","grid","gridExtra","ggrepel")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


###############################################################################################################
###############################################################################################################
# FEMALES
###############################################################################################################
###############################################################################################################


FAMD_kmeans_ordinal_factors_clustering_female=
  readRDS("../real_results_from_HPC/results_joel_HPC_V3_female/FAMD_kmeans_ordinal_factors/FAMD_kmeans_ordinal_factors_multi_morbid.rds")


gower_diana_multi_morbid_clustering_female=
  readRDS("../real_results_from_HPC/results_joel_HPC_V3_female/gower_diana/gower_diana_multi_morbid.rds")


kamila_multi_morbid_clustering_female=
  readRDS("../real_results_from_HPC/results_joel_HPC_V3_female/kamila_ordinal_factors/kamila_kamila_multi_morbid.rds")


multi_morbid_ordinal_factors_HW_mod_female=readRDS("../data/processed_V3_females/multi_morbid_ordinal_factors_HW_mod_female.rds")

multi_morbid_ordinal_factors_HW_mod_female_subset=readRDS("../data/processed_V3_females/multi_morbid_ordinal_factors_HW_mod_female_subset.rds")


disease_variables=colnames(multi_morbid_ordinal_factors_HW_mod_female)[2:10]
disease_variables=disease_variables[-6]



distribution_test_df=data.frame(matrix(0,ncol=2,nrow=length(disease_variables)))
colnames(distribution_test_df)=c("var_name","p_value")
distribution_test_df[,1]=disease_variables



###########

for (k in 1:nrow(distribution_test_df)) {
  
  distribution_test_df[k,2]=chisq.test(multi_morbid_ordinal_factors_HW_mod_female[,distribution_test_df[k,1]],as.factor(FAMD_kmeans_ordinal_factors_clustering_female$cluster))$p.value
  
}

distribution_test_df[,2]=p.adjust(distribution_test_df[,2],method="bonferroni")

saveRDS(distribution_test_df,"../real_results_from_HPC/results_tables_final_figures/distribution_test_df_FAMD_kmeans_ordinal_factors_female.rds")

###########

for (k in 1:nrow(distribution_test_df)) {
  
  distribution_test_df[k,2]=chisq.test(multi_morbid_ordinal_factors_HW_mod_female[,distribution_test_df[k,1]],
                                       as.factor(kamila_multi_morbid_clustering_female$finalMemb))$p.value
  
}

distribution_test_df[,2]=p.adjust(distribution_test_df[,2],method="bonferroni")

saveRDS(distribution_test_df,"../real_results_from_HPC/results_tables_final_figures/distribution_test_df_kamila_ordinal_factors_female.rds")

###########

for (k in 1:nrow(distribution_test_df)) {
  
  distribution_test_df[k,2]=chisq.test(multi_morbid_ordinal_factors_HW_mod_female_subset[,distribution_test_df[k,1]],
                                       as.factor(cutree(gower_diana_multi_morbid_clustering_female,2)))$p.value
  
}

distribution_test_df[,2]=p.adjust(distribution_test_df[,2],method="bonferroni")

saveRDS(distribution_test_df,"../real_results_from_HPC/results_tables_final_figures/distribution_test_df_gower_diana_female.rds")



###############################################################################################################
###############################################################################################################
# MALES
###############################################################################################################
###############################################################################################################




FAMD_kmeans_ordinal_factors_clustering_male=
  readRDS("../real_results_from_HPC/results_joel_HPC_V3_male/FAMD_kmeans_ordinal_factors/FAMD_kmeans_ordinal_factors_multi_morbid.rds")


gower_diana_multi_morbid_clustering_male=
  readRDS("../real_results_from_HPC/results_joel_HPC_V3_male/gower_diana/gower_diana_multi_morbid.rds")


kamila_multi_morbid_clustering_male=
  readRDS("../real_results_from_HPC/results_joel_HPC_V3_male/kamila_ordinal_factors/kamila_kamila_multi_morbid.rds")


multi_morbid_ordinal_factors_HW_mod_male=readRDS("../data/processed_V3_males/multi_morbid_ordinal_factors_HW_mod_male.rds")

multi_morbid_ordinal_factors_HW_mod_male_subset=readRDS("../data/processed_V3_males/multi_morbid_ordinal_factors_HW_mod_male_subset.rds")


###########


for (k in 1:nrow(distribution_test_df)) {
  
  distribution_test_df[k,2]=chisq.test(multi_morbid_ordinal_factors_HW_mod_male[,distribution_test_df[k,1]],as.factor(FAMD_kmeans_ordinal_factors_clustering_male$cluster))$p.value
  
}

distribution_test_df[,2]=p.adjust(distribution_test_df[,2],method="bonferroni")

saveRDS(distribution_test_df,"../real_results_from_HPC/results_tables_final_figures/distribution_test_df_FAMD_kmeans_ordinal_factors_male.rds")

###########

for (k in 1:nrow(distribution_test_df)) {
  
  distribution_test_df[k,2]=chisq.test(multi_morbid_ordinal_factors_HW_mod_male[,distribution_test_df[k,1]],
                                       as.factor(kamila_multi_morbid_clustering_male$finalMemb))$p.value
  
}

distribution_test_df[,2]=p.adjust(distribution_test_df[,2],method="bonferroni")

saveRDS(distribution_test_df,"../real_results_from_HPC/results_tables_final_figures/distribution_test_df_kamila_ordinal_factors_male.rds")

###########

for (k in 1:nrow(distribution_test_df)) {
  
  distribution_test_df[k,2]=chisq.test(multi_morbid_ordinal_factors_HW_mod_male_subset[,distribution_test_df[k,1]],
                                       as.factor(cutree(gower_diana_multi_morbid_clustering_male,2)))$p.value
  
}

distribution_test_df[,2]=p.adjust(distribution_test_df[,2],method="bonferroni")

saveRDS(distribution_test_df,"../real_results_from_HPC/results_tables_final_figures/distribution_test_df_gower_diana_male.rds")
