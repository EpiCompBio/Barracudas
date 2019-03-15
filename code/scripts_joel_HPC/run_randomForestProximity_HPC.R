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


# full_data=read.csv("../data/processed/UKBcompleteFeb19_subset.csv",row.names=1)
full_data=read.csv("../data/processed/UKBcompleteFeb19.csv")



################################################################################
# PRE-PROCESSING
################################################################################

#define obese BMI > 35
full_data$obese = ifelse(full_data$BMI >= 35, 1, 0)

#define outcome cols
outcomes = c('diabetes','mi','stroke','angina','obese')

outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(full_data))

#col of chronic diseases
full_data$no_chronic = apply(full_data[,outcome_cols],1,sum)

#change gender levels and remove gender that is not used anymore
full_data$Sex = factor(ifelse(full_data$gender == 0, 'Female','Male'))
full_data$gender=NULL

#binary cols
binary_cols = which(unlist(sapply(full_data, function(x) length(levels(factor(x)))==2)))
full_data[,binary_cols]=lapply(full_data[,binary_cols],as.factor)

#re-organize columns
full_data=full_data %>% dplyr::select(eid,mi,angina,stroke,htn,obese,no_chronic, everything())

#subset multi morbid rows
multi_morbid = full_data[which(full_data$no_chronic>1),]

full_data[,'no_chronic']=as.factor(full_data[,'no_chronic'])
multi_morbid[,'no_chronic']=as.factor(multi_morbid[,'no_chronic'])


for (k in 1:ncol(full_data)) {
  if (class(full_data[,k])!="factor" & k!=1) {
    full_data[,k]=scale(full_data[,k])
  }
}

for (k in 1:ncol(multi_morbid)) {
  if (class(multi_morbid[,k])!="factor" & k!=1) {
    multi_morbid[,k]=scale(multi_morbid[,k])
  }
}


################################################################################
################################################################################
# multi-morbid individuals only
################################################################################
################################################################################

################################################################################
# Random forests for the proximty measures
################################################################################


RF_proximity_measure_multi_morbid_res=randomForest(x=multi_morbid)$proximity

saveRDS(RF_proximity_measure_multi_morbid_res,"../data/processed/RF_proximity_measure_multi_morbid_res.rds")


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
# saveRDS(RF_proximity_measure_full_data_res,"../data/processed/RF_proximity_measure_full_data_res.rds")

