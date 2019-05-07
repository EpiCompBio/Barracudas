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

using("ggplot2","grid","gridExtra","ggrepel","dplyr","reshape2")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


source("C:/Users/JOE/Documents/R_utility_and_self_implementations/colors_themes_utility.R")

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
################################################################################
# LOAD NECESSARY RESULTS
################################################################################


cluster_crit_df_FAMD_kmeans_ordinal_factors_multi_morbid_female=
  readRDS("../real_results_from_HPC/results_joel_HPC_V5_female/FAMD_kmeans_ordinal_factors/cluster_crit_df_FAMD_kmeans_ordinal_factors_multi_morbid.rds")

cluster_crit_df_FAMD_kmeans_ordinal_factors_multi_morbid_male=
  readRDS("../real_results_from_HPC/results_joel_HPC_V5_male/FAMD_kmeans_ordinal_factors/cluster_crit_df_FAMD_kmeans_ordinal_factors_multi_morbid.rds")




cluster_crit_data=data.frame(cluster_crit_df_FAMD_kmeans_ordinal_factors_multi_morbid_female[,1:2],
                             cluster_crit_df_FAMD_kmeans_ordinal_factors_multi_morbid_male$Cal_Har)

colnames(cluster_crit_data)=c("n_classes","Cal_Har_female","Cal_Har_male")

cluster_crit_data_melted=melt(cluster_crit_data,id.vars='n_classes')


cluster_crit_plot=ggplot(cluster_crit_data_melted) + geom_line(aes(x=n_classes,y=value,color=variable),size=1) + theme_jh + 
  scale_color_manual("Calinski Harabasz", label=c("Female","Male"),values=gg_color_hue(2),
                     breaks=levels(cluster_crit_data_melted$variable)) + ylab("Value") + xlab("Number of classes")



x11()
print(cluster_crit_plot)