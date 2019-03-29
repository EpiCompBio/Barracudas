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

# setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")

################################################################################
################################################################################
# multi-morbid individuals only
################################################################################
################################################################################

multi_morbid=readRDS("../data/processed_V2/multi_morbid_male_ordinal_keep.rds")
# multi_morbid=multi_morbid[1:200,]

################################################################################
# Random forests for the proximty measures
################################################################################


RF_proximity_measure_multi_morbid_res=randomForest(x=multi_morbid)$proximity

saveRDS(RF_proximity_measure_multi_morbid_res,"../data/processed_V2/RF_proximity_measure_ordinal_factors_multi_morbid_male_res.rds")


################################################################################
################################################################################
# full dataset MALE
################################################################################
################################################################################

################################################################################
# Random forests for the proximty measures
################################################################################


# RF_proximity_measure_full_data_res=randomForest(x=full_data)$proximity
# 
# saveRDS(RF_proximity_measure_full_data_res,"../data/processed/RF_proximity_measure_ordinal_continuous_full_data_res.rds")




################################################################################
################################################################################
# multi-morbid individuals only
################################################################################
################################################################################

multi_morbid=readRDS("../data/processed_V2/multi_morbid_female_ordinal_keep.rds")
# multi_morbid=multi_morbid[1:200,]

################################################################################
# Random forests for the proximty measures
################################################################################


RF_proximity_measure_multi_morbid_res=randomForest(x=multi_morbid)$proximity

saveRDS(RF_proximity_measure_multi_morbid_res,"../data/processed_V2/RF_proximity_measure_ordinal_factors_multi_morbid_female_res.rds")


################################################################################
################################################################################
# full dataset FEMALE
################################################################################
################################################################################

################################################################################
# Random forests for the proximty measures
################################################################################


# RF_proximity_measure_full_data_res=randomForest(x=full_data)$proximity
# 
# saveRDS(RF_proximity_measure_full_data_res,"../data/processed/RF_proximity_measure_ordinal_continuous_full_data_res.rds")
