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
# using("randomForest")


library(randomForest,lib.loc ="/home/jheller/anaconda3/lib/R/library")


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


multi_morbid=readRDS("../data/processed/multi_morbid_ordinal_continuous_subset.rds")
# multi_morbid=multi_morbid[1:200,]

################################################################################
# Random forests for the proximty measures
################################################################################


RF_proximity_measure_multi_morbid_res=randomForest(x=multi_morbid[,16:ncol(multi_morbid)])$proximity

saveRDS(RF_proximity_measure_multi_morbid_res,"../data/processed/RF_proximity_measure_ordinal_factors_multi_morbid_res.rds")


################################################################################
################################################################################
# full dataset
################################################################################
################################################################################

################################################################################
# Random forests for the proximty measures
################################################################################


# RF_proximity_measure_full_data_res=randomForest(x=full_data)$proximity
# 
# saveRDS(RF_proximity_measure_full_data_res,"../data/processed/RF_proximity_measure_ordinal_factors_full_data_res.rds")
