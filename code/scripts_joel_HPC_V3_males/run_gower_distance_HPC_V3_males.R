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

# setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")

################################################################################
################################################################################
# multi-morbid individuals only
################################################################################
################################################################################

multi_morbid=readRDS("../data/processed_V3_males/multi_morbid_ordinal_factors_HW_mod_male_subset.rds")
# multi_morbid=multi_morbid[1:200,]

################################################################################
# gower distance for the proximty measures
################################################################################

gower_dissimilarity_multi_morbid_res=as.matrix(daisy(multi_morbid[,15:ncol(multi_morbid)],metric="gower"))


saveRDS(gower_dissimilarity_multi_morbid_res,"../data/processed_V3_males/gower_dissimilarity_multi_morbid_res.rds")

