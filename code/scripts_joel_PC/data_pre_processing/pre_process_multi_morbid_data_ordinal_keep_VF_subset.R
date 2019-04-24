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

set.seed(1)
################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")

################################################################################
# LOADING DATA
################################################################################

full_data=read.csv("../data/processed/UKBcompleteFeb19.csv")

deborah_data=read.csv("../data/processed/CVD_diag_by_doc_236.csv",sep=";")
colnames(deborah_data)[1]="eid"

CVD_ICD10_codes=read.table("../data/processed/CVD_icd10codes.txt")[,1]

deborah_data2=read.csv("../data/processed/CVD_10035_236.csv")
colnames(deborah_data2)[ncol(deborah_data2)-1]="eid"

print(table(deborah_data2[,2],useNA="ifany"))
print(table(deborah_data2[,3],useNA="ifany"))

#These NAs are just from people that never had CVD, so we'll code as 0
print(length(deborah_data2[deborah_data2$first_admidate=="","first_admidate"]))


deborah_data2[is.na(deborah_data2)] <- 0

print(table(deborah_data2[,2],useNA="ifany"))
print(table(deborah_data2[,3],useNA="ifany"))

################################################################################
# DEALING WITH ICD10 CODE
################################################################################

#These are the ICD10 codes of all the diseases in the dataset
ICD10_code_names=colnames(deborah_data2)[2:76]

#Let's check how many of them are in the list of CVD related ICD10 codes
print(ICD10_code_names%in%CVD_ICD10_codes)

print(ICD10_code_names[!ICD10_code_names%in%CVD_ICD10_codes]) #All but one!
#I500 : corresponds to forms of heart failure. Note that I50 is in list so we'll keep it in still


print(unique(substr(ICD10_code_names, start = 1, stop = 3)))

#Here are the disease types that correspond to the codes

#I22: MI
#I61 : Intracerebral haemorrhage
#I21 : MI
#I25: Chronic ischaemic heart diseases
#I50: Heart failure
#I63: Cerebral infarction
#G46: Vascular syndromes of brain in cerebrovascular diseases
#I60: Subarachnoid haemorrhage
#I24: Other acute ischaemic heart diseases
#I20 : Angina pectoris
#I62 : Other nontraumatic intracranial haemorrhage
#I23: Certain current complications following acute myocardial infarction
#I69 : Sequelae of cerebrovascular disease
#I73:Other peripheral vascular diseases
#I64:Stroke, not specified as haemorrhage or infarction

#We now try to make meaningful groups
CAD_codes=c("I22","I21","I23","I25","I20","I24")
stroke_codes=c("I63","I64","I69","G46")
heart_failure_codes=c("I50")
intracranial_haemorrhage_codes=c("I61","I60","I62")
peripheral_vascular_codes=c("I73")


CAD_data=ifelse(rowSums(deborah_data2[,substr(colnames(deborah_data2), start = 1, stop = 3)%in%CAD_codes])>=1,1,0)
stroke_data=ifelse(rowSums(deborah_data2[,substr(colnames(deborah_data2), start = 1, stop = 3)%in%stroke_codes])>=1,1,0)
heart_failure_data=ifelse(rowSums(deborah_data2[,substr(colnames(deborah_data2), start = 1, stop = 3)%in%heart_failure_codes])>=1,1,0)
intracranial_haemorrhage_data=ifelse(rowSums(deborah_data2[,substr(colnames(deborah_data2), start = 1, stop = 3)%in%intracranial_haemorrhage_codes])>=1,1,0)
peripheral_vascular_data=ifelse(rowSums(deborah_data2[,substr(colnames(deborah_data2), start = 1, stop = 3)%in%peripheral_vascular_codes])>=1,1,0)


supp_data=data.frame(CAD_data,stroke_data,heart_failure_data,intracranial_haemorrhage_data,peripheral_vascular_data,eid=deborah_data2$eid)

merged_data=merge(full_data,deborah_data,by="eid")
merged_data=merge(merged_data,supp_data,by="eid")

merged_data$mi=NULL
merged_data$angina=NULL
merged_data$stroke=NULL
merged_data$htn=NULL

#SHow how much HES data corresponds to self reported

merged_data$CAD=ifelse(merged_data$angina_diag+merged_data$heartattack_diag+merged_data$CAD_data>=1,1,0)
merged_data$stroke=ifelse(merged_data$stroke_diag+merged_data$stroke_data>=1,1,0)
merged_data$heart_failure=merged_data$heart_failure_data
merged_data$intracranial_haemorrhage=merged_data$intracranial_haemorrhage_data
merged_data$peripheral_vascular=merged_data$peripheral_vascular_data


merged_data$heartattack_diag=NULL
merged_data$angina_diag=NULL
merged_data$stroke_diag=NULL


merged_data$CAD_data=NULL
merged_data$heart_failure_data=NULL
merged_data$stroke_data=NULL
merged_data$intracranial_haemorrhage_data=NULL 
merged_data$peripheral_vascular_data=NULL 


merged_data$htn=merged_data$hBP_diag
merged_data$hBP_diag=NULL


merged_data$htn=as.numeric(as.character(merged_data$htn))


table(merged_data$htn)
table(merged_data$intracranial_haemorrhage)
table(merged_data$peripheral_vascular)


UKB_age=readRDS("../data/processed/UKB_age.rds")
colnames(UKB_age)=c("eid","age")

merged_data=merge(merged_data,UKB_age,by="eid")
merged_data$birth_year=NULL


################################################################################
# PRE-PROCESSING ON FULL DATA
################################################################################

#define obese BMI > 40
merged_data$obese = ifelse(merged_data$BMI >= 40, 1, 0)

#define outcome cols
outcomes = c('diabetes','CAD','angina','obese','htn',"heart_failure","intracranial_haemorrhage","peripheral_vascular")



outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(merged_data))


#col of chronic diseases
merged_data$no_chronic = apply(merged_data[,outcome_cols],1,sum)


#change gender levels and remove gender that is not used anymore
merged_data$Sex = factor(ifelse(merged_data$gender == 0, 'Female','Male'))
merged_data$gender=NULL

#re-organize columns
merged_data=merged_data %>% dplyr::select(eid,CAD,stroke,obese,diabetes,htn,dvt_asthma_copd_atopy,
                                          heart_failure,intracranial_haemorrhage,peripheral_vascular,no_chronic,self_reported_surgery,
                                          previous_surgery,pacemaker, everything())


merged_data[,'no_chronic']=as.factor(merged_data[,'no_chronic'])

#binary cols
binary_col_ids = which(unlist(sapply(merged_data, function(x) length(levels(factor(x)))==2)))
merged_data[,binary_col_ids]=lapply(merged_data[,binary_col_ids],as.factor)

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

cat_col_ids = which(colnames(merged_data) %in% cat_cols)
ord_col_ids = which(colnames(merged_data) %in% ord_cols)


merged_data[,cat_col_ids] = factor(merged_data[,cat_col_ids])
merged_data[,ord_col_ids] = lapply(merged_data[,ord_col_ids], function(x) factor(as.integer(x), ordered = TRUE))


####################################################################################################
# PREPARING MULTI-MORBID DATA AND SCALING
####################################################################################################

#subset multi morbid rows
multi_morbid = merged_data[which(as.numeric(as.character(merged_data$no_chronic))>1),]

multi_morbid <- multi_morbid %>%
  group_by(Sex,age) %>%
  sample_frac(0.3) %>% as.data.frame()


#scale numeric features
multi_morbid[,-c(1,11,binary_col_ids,cat_col_ids,ord_col_ids)] =
  as.data.frame(scale(multi_morbid[,-c(1,11,binary_col_ids,cat_col_ids,ord_col_ids)]))



saveRDS(multi_morbid,"../data/processed/multi_morbid_ordinal_keep_subset.rds")