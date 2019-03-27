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

using("magrittr","dplyr")

# library(cluster,lib.loc ="/home/jheller/anaconda3/lib/R/library")
# library(dplyr,lib.loc ="/home/jheller/anaconda3/lib/R/library")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

# file_path<-dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(file_path)

setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


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

#re-organize columns
full_data=full_data %>% dplyr::select(eid,mi,angina,stroke,obese,diabetes,htn,dvt_asthma_copd_atopy,no_chronic, everything())


full_data[,'no_chronic']=as.factor(full_data[,'no_chronic'])

#binary cols
binary_col_ids = which(unlist(sapply(full_data, function(x) length(levels(factor(x)))==2)))
full_data[,binary_col_ids]=lapply(full_data[,binary_col_ids],as.factor)

#We'll transform all the ordinal columns so that they can reasonably be considered continuous
ord_cols = c('self_reported_surgery',
             'freq_climb_stairs_4wks',
             'freq_walked_for_pleasure_4wks',
             'Duration_pleasure_walks',
             'smokers_in_house',
             'oily_fish_intake',
             'non_oily_fish_intake',
             'processed_meat',
             'poultry',
             'beef_intake',
             'lamb_intake',
             'pork_intake',
             'cheese_intake',
             'salt_added_food',
             'varition_in_diet',
             'Alc_intake_freq',
             'seated_box_height'
)

#categorical cols
cat_cols = c('birth_month')

cat_col_ids = which(colnames(full_data) %in% cat_cols)
ord_col_ids = which(colnames(full_data) %in% ord_cols)


full_data[,cat_col_ids] = factor(full_data[,cat_col_ids])
full_data[,ord_col_ids] = lapply(full_data[,ord_col_ids], function(x) factor(as.integer(x), ordered = TRUE))


####################################################################################################
# PREPARING MULTI-MORBID DATA AND SCALING
####################################################################################################

#subset multi morbid rows
multi_morbid = full_data[which(as.numeric(as.character(full_data$no_chronic))>1),]

multi_morbid_male=multi_morbid[multi_morbid$Sex=="Male",]
multi_morbid_female=multi_morbid[multi_morbid$Sex=="Female",]


multi_morbid_male$Sex=NULL
multi_morbid_female$Sex=NULL


full_data_male=full_data[full_data$Sex=="Male",]
full_data_female=full_data[full_data$Sex=="Female",]


full_data_male$Sex=NULL
full_data_female$Sex=NULL

binary_col_ids=binary_col_ids[-length(binary_col_ids)]


#scale numeric features
full_data_female[,-c(1,9,binary_col_ids,cat_col_ids,ord_col_ids)] =
  as.data.frame(scale(full_data_female[,-c(1,9,binary_col_ids,cat_col_ids,ord_col_ids)]))



#scale numeric features
full_data_male[,-c(1,9,binary_col_ids,cat_col_ids,ord_col_ids)] =
  as.data.frame(scale(full_data_male[,-c(1,9,binary_col_ids,cat_col_ids,ord_col_ids)]))



#scale numeric features
multi_morbid_male[,-c(1,9,binary_col_ids,cat_col_ids,ord_col_ids)] =
  as.data.frame(scale(multi_morbid_male[,-c(1,9,binary_col_ids,cat_col_ids,ord_col_ids)]))

#scale numeric features
multi_morbid_female[,-c(1,9,binary_col_ids,cat_col_ids,ord_col_ids)] =
  as.data.frame(scale(multi_morbid_female[,-c(1,9,binary_col_ids,cat_col_ids,ord_col_ids)]))



saveRDS(full_data_male,"../data/processed_V2/full_data_male_ordinal_keep.rds")
saveRDS(full_data_female,"../data/processed_V2/full_data_female_ordinal_keep.rds")


saveRDS(multi_morbid_male,"../data/processed_V2/multi_morbid_male_ordinal_keep.rds")
saveRDS(multi_morbid_female,"../data/processed_V2/multi_morbid_female_ordinal_keep.rds")












