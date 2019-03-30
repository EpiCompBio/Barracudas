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

setwd("/Users/abieast/Documents/Imperial/Translational_Data_Science/Project")


# full_data=read.csv("../data/processed/UKBcompleteFeb19_subset.csv",row.names=1)
full_data_unscaled=read.csv("Data/UKBcompleteFeb19.csv")



################################################################################
# PRE-PROCESSING
################################################################################

#Remove asthma variable
full_data_unscaled$dvt_asthma_copd_atopy <- NULL

#define obese BMI > 40
full_data_unscaled$obese = ifelse(full_data_unscaled$BMI >= 40, 1, 0)

#define outcome cols
outcomes = c('diabetes','mi','stroke','angina','obese', 'htn')

outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(full_data_unscaled))

#col of chronic diseases
full_data_unscaled$no_chronic = apply(full_data_unscaled[,outcome_cols],1,sum)

#change gender levels and remove gender that is not used anymore
full_data_unscaled$Sex = factor(ifelse(full_data_unscaled$gender == 0, 'Female','Male'))
full_data_unscaled$gender=NULL

#re-organize columns
full_data_unscaled=full_data_unscaled %>% dplyr::select(eid,mi,angina,stroke,obese,diabetes,htn,no_chronic, everything())


full_data_unscaled[,'no_chronic']=as.factor(full_data_unscaled[,'no_chronic'])

#binary cols
binary_col_ids = which(unlist(sapply(full_data_unscaled, function(x) length(levels(factor(x)))==2)))
full_data_unscaled[,binary_col_ids]=lapply(full_data_unscaled[,binary_col_ids],as.factor)

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

cat_col_ids = which(colnames(full_data_unscaled) %in% cat_cols)
ord_col_ids = which(colnames(full_data_unscaled) %in% ord_cols)


full_data_unscaled[,cat_col_ids] = factor(full_data_unscaled[,cat_col_ids])
full_data_unscaled[,ord_col_ids] = lapply(full_data_unscaled[,ord_col_ids], function(x) factor(as.integer(x), ordered = TRUE))


####################################################################################################
# PREPARING MULTI-MORBID DATA AND SCALING
####################################################################################################

#subset multi morbid rows
multi_morbid_unscaled = full_data_unscaled[which(as.numeric(as.character(full_data_unscaled$no_chronic))>1),]

# Set eid as rownames
rownames(multi_morbid_unscaled) <- multi_morbid_unscaled[,1]
multi_morbid_unscaled[,1] <- NULL

# Remove outcome columns
multi_morbid_unscaled <- multi_morbid_unscaled[ ,-c(1:7)]

# Reorder columns
multi_morbid_unscaled <- multi_morbid_unscaled[ , c(1, 2, 3, 4, 5, 7, 9, 10,11,12,13,14,15,19,20,21,23,24,25,26,27,28,37,38,40,41,42,46,47,48,49,
                                  50,51,52,53,6,8,16,17,18,22,29,30,31,32,33,34,35,36,39,43,44,45,54,55,56,57,58,59,60,61,62,
                                  63,64,65,66,67,68,69,70)]

saveRDS(multi_morbid_unscaled, "Data/Version2/multi_morbid_unscaled_ordinal_factors.rds")

#scale numeric features
multi_morbid_unscaled[,1:35] =
  as.data.frame(scale(multi_morbid_unscaled[,1:35]))

saveRDS(multi_morbid_unscaled, "Data/Version2/multi_morbid_scaled_ordinal_factors.rds")



#### Doesn't contain age, still contains sitting height and birth month/year ####