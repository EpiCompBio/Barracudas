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


# library(magrittr,lib.loc ="/home/jheller/anaconda3/lib/R/library")
# library(dplyr,lib.loc ="/home/jheller/anaconda3/lib/R/library")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

# file_path<-dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(file_path)

setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")



full_data=read.csv("../data/processed/UKBcompleteFeb19.csv")

################################################################################
# PRE-PROCESSING ON FULL DATA
################################################################################

#define obese BMI > 40
full_data$obese = ifelse(full_data$BMI >= 40, 1, 0)

#define outcome cols
outcomes = c('diabetes','mi','stroke','angina','obese','htn')

outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(full_data))


#col of chronic diseases
full_data$no_chronic = apply(full_data[,outcome_cols],1,sum)

#change gender levels and remove gender that is not used anymore
full_data$Sex = factor(ifelse(full_data$gender == 0, 'Female','Male'))
full_data$gender=NULL

#re-organize columns
full_data=full_data %>% dplyr::select(eid,mi,angina,stroke,obese,diabetes,htn,dvt_asthma_copd_atopy,no_chronic, everything())


full_data[,'no_chronic']=as.factor(full_data[,'no_chronic'])
full_data[,'birth_month']=as.factor(full_data[,'birth_month'])


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


# This is OK for a continuous variable (it's count data)
print(summary(full_data[,'self_reported_surgery']))

# This is not ideal, we'll use the information from UK Biobank to recode
print(table(full_data[,'freq_climb_stairs_4wks']))
print(summary(full_data[,'freq_climb_stairs_4wks']))
full_data[,'freq_climb_stairs_4wks']=recode(full_data[,'freq_climb_stairs_4wks'],"0"=0,"1"=3,"2"=8,"3"=13,"4"=18,"5"=25)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(full_data[,'freq_walked_for_pleasure_4wks']))
print(summary(full_data[,'freq_walked_for_pleasure_4wks']))
full_data[,'freq_walked_for_pleasure_4wks']=recode(full_data[,'freq_walked_for_pleasure_4wks'],"1"=1,"2"=2.5,"3"=4,"4"=10,"5"=18,"6"=28)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(full_data[,'Duration_pleasure_walks']))
print(summary(full_data[,'Duration_pleasure_walks']))
full_data[,'Duration_pleasure_walks']=recode(full_data[,'Duration_pleasure_walks'],"1"=7.5,"2"=22.5,"3"=45,"4"=75,"5"=105,"6"=150,
                                             "7"=240)

# This is alright for continuous
print(table(full_data[,'smokers_in_house']))
print(summary(full_data[,'smokers_in_house']))


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(full_data[,'oily_fish_intake']))
print(summary(full_data[,'oily_fish_intake']))
full_data[,'oily_fish_intake']=recode(full_data[,'oily_fish_intake'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(full_data[,'non_oily_fish_intake']))
print(summary(full_data[,'non_oily_fish_intake']))
full_data[,'non_oily_fish_intake']=recode(full_data[,'non_oily_fish_intake'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(full_data[,'processed_meat']))
print(summary(full_data[,'processed_meat']))
full_data[,'processed_meat']=recode(full_data[,'processed_meat'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(full_data[,'poultry']))
print(summary(full_data[,'poultry']))
full_data[,'poultry']=recode(full_data[,'poultry'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(full_data[,'beef_intake']))
print(summary(full_data[,'beef_intake']))
full_data[,'beef_intake']=recode(full_data[,'beef_intake'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(full_data[,'lamb_intake']))
print(summary(full_data[,'lamb_intake']))
full_data[,'lamb_intake']=recode(full_data[,'lamb_intake'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(full_data[,'pork_intake']))
print(summary(full_data[,'pork_intake']))
full_data[,'pork_intake']=recode(full_data[,'pork_intake'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(full_data[,'cheese_intake']))
print(summary(full_data[,'cheese_intake']))
full_data[,'cheese_intake']=recode(full_data[,'cheese_intake'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(full_data[,'salt_added_food']))
print(summary(full_data[,'salt_added_food']))
full_data[,'salt_added_food']=recode(full_data[,'salt_added_food'],"1"=0,"2"=1,"3"=2,"4"=3)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(full_data[,'varition_in_diet']))
print(summary(full_data[,'varition_in_diet']))
full_data[,'varition_in_diet']=recode(full_data[,'varition_in_diet'],"1"=0,"2"=1,"3"=2)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(full_data[,'Alc_intake_freq']))
print(summary(full_data[,'Alc_intake_freq']))
full_data[,'Alc_intake_freq']=recode(full_data[,'Alc_intake_freq'],"1"=28,"2"=14,"3"=6,"4"=2,"5"=1,"6"=0)


# Let's say this is OK! 
print(table(full_data[,'seated_box_height']))
print(summary(full_data[,'seated_box_height']))

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

for (k in 1:ncol(full_data_male)) {
  if (class(full_data_male[,k])!="factor" & k!=1) {
    full_data_male[,k]=as.numeric(scale(full_data_male[,k]))
  }
}

for (k in 1:ncol(full_data_female)) {
  if (class(full_data_female[,k])!="factor" & k!=1) {
    full_data_female[,k]=as.numeric(scale(full_data_female[,k]))
  }
}


for (k in 1:ncol(multi_morbid_female)) {
  if (class(multi_morbid_female[,k])!="factor" & k!=1) {
    multi_morbid_female[,k]=as.numeric(scale(multi_morbid_female[,k]))
  }
}

for (k in 1:ncol(multi_morbid_male)) {
  if (class(multi_morbid_male[,k])!="factor" & k!=1) {
    multi_morbid_male[,k]=as.numeric(scale(multi_morbid_male[,k]))
  }
}

saveRDS(full_data_male,"../data/processed_V2/full_data_male_ordinal_continuous_male.rds")
saveRDS(full_data_female,"../data/processed_V2/full_data_female_ordinal_continuous_male.rds")


saveRDS(multi_morbid_male,"../data/processed_V2/multi_morbid_male_ordinal_continuous.rds")
saveRDS(multi_morbid_female,"../data/processed_V2/multi_morbid_female_ordinal_continuous.rds")



