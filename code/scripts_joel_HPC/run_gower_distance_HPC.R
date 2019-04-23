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

################################################################################
################################################################################
# multi-morbid individuals only
################################################################################
################################################################################


multi_morbid=readRDS("../data/processed/multi_morbid_ordinal_keep_subset.rds")
# multi_morbid=multi_morbid[1:200,]

################################################################################
# gower distance for the proximty measures
################################################################################

gower_dissimilarity_multi_morbid_res=as.matrix(daisy(multi_morbid[,16:ncol(multi_morbid)],metric="gower"))


saveRDS(gower_dissimilarity_multi_morbid_res,"../data/processed/gower_dissimilarity_multi_morbid_res.rds")


################################################################################
################################################################################
# full dataset
################################################################################
################################################################################

################################################################################
# gower distance for the proximty measures
################################################################################

# gower_dissimilarity_full_data_res=as.matrix(daisy(full_data,metric="gower"))
# 
# 
# saveRDS(gower_dissimilarity_full_data_res,"../data/processed/gower_dissimilarity_full_data_res.rds")



