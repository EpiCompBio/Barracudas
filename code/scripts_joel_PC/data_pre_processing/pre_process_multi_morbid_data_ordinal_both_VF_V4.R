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

# file_path<-dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(file_path)

setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")

multi_morbid_ordinal_continuous=readRDS("../data/processed/multi_morbid_ordinal_continuous.rds")

multi_morbid_ordinal_factors=readRDS("../data/processed/multi_morbid_ordinal_keep.rds")

# We noticed that the columns "smoker" and "current_smoker" are the  (just different for 5 individuals), so we're going to remove one
print(sum(multi_morbid_ordinal_continuous$smoker == multi_morbid_ordinal_continuous$current_smoker))
print(nrow(multi_morbid_ordinal_continuous))

# We're going to remove one of them
multi_morbid_ordinal_continuous$smoker=NULL
multi_morbid_ordinal_factors$smoker=NULL

#Birth_month is a dumb variable
multi_morbid_ordinal_continuous$birth_month=NULL
multi_morbid_ordinal_factors$birth_month=NULL

multi_morbid_ordinal_continuous$hip_waist_ratio=multi_morbid_ordinal_continuous$hip_circum/multi_morbid_ordinal_continuous$waist_circum
multi_morbid_ordinal_factors$hip_waist_ratio=multi_morbid_ordinal_factors$hip_circum/multi_morbid_ordinal_factors$waist_circum

multi_morbid_ordinal_continuous$height_sitting=NULL
multi_morbid_ordinal_factors$height_sitting=NULL

multi_morbid_ordinal_continuous$sitting_height=NULL
multi_morbid_ordinal_factors$sitting_height=NULL


height_weight_related=c("Height","height_sitting","sitting_height","BMI",
                        "Weight","body_fat_perc","whole_body_fat_mass","whole_body_water_mass","waist_circum","hip_circum")


multi_morbid_HW=multi_morbid_ordinal_continuous[,height_weight_related]


PCA_multi_morbid_HW=PCA(multi_morbid_HW)


multi_morbid_ordinal_continuous[,height_weight_related] <- list(NULL)

multi_morbid_ordinal_factors[,height_weight_related] <- list(NULL)


multi_morbid_ordinal_continuous[,"seated_box_height"] <- list(NULL)
multi_morbid_ordinal_factors[,"seated_box_height"] <- list(NULL)

multi_morbid_ordinal_continuous$HW_PCA_PC1=PCA_multi_morbid_HW$ind$coord[,1]
multi_morbid_ordinal_continuous$HW_PCA_PC2=PCA_multi_morbid_HW$ind$coord[,2]

multi_morbid_ordinal_factors$HW_PCA_PC1=PCA_multi_morbid_HW$ind$coord[,1]
multi_morbid_ordinal_factors$HW_PCA_PC2=PCA_multi_morbid_HW$ind$coord[,2]

multi_morbid_ordinal_factors[,c('HW_PCA_PC1','HW_PCA_PC2')]=scale(multi_morbid_ordinal_factors[,c('HW_PCA_PC1','HW_PCA_PC2')])
multi_morbid_ordinal_continuous[,c('HW_PCA_PC1','HW_PCA_PC2')]=scale(multi_morbid_ordinal_continuous[,c('HW_PCA_PC1','HW_PCA_PC2')])



saveRDS(multi_morbid_ordinal_continuous,"../data/processed_V5/multi_morbid_ordinal_continuous_HW_mod.rds")
saveRDS(multi_morbid_ordinal_factors,"../data/processed_V5/multi_morbid_ordinal_factors_HW_mod.rds")



