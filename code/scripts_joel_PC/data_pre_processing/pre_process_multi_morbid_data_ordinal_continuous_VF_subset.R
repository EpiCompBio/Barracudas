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
merged_data[,'birth_month']=as.factor(merged_data[,'birth_month'])


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


# This is OK for a continuous variable (it's count data)
print(summary(merged_data[,'self_reported_surgery']))

# This is not ideal, we'll use the information from UK Biobank to recode
print(table(merged_data[,'freq_climb_stairs_4wks']))
print(summary(merged_data[,'freq_climb_stairs_4wks']))
merged_data[,'freq_climb_stairs_4wks']=recode(merged_data[,'freq_climb_stairs_4wks'],"0"=0,"1"=3,"2"=8,"3"=13,"4"=18,"5"=25)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(merged_data[,'freq_walked_for_pleasure_4wks']))
print(summary(merged_data[,'freq_walked_for_pleasure_4wks']))
merged_data[,'freq_walked_for_pleasure_4wks']=recode(merged_data[,'freq_walked_for_pleasure_4wks'],"1"=1,"2"=2.5,"3"=4,"4"=10,"5"=18,"6"=28)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(merged_data[,'Duration_pleasure_walks']))
print(summary(merged_data[,'Duration_pleasure_walks']))
merged_data[,'Duration_pleasure_walks']=recode(merged_data[,'Duration_pleasure_walks'],"1"=7.5,"2"=22.5,"3"=45,"4"=75,"5"=105,"6"=150,
                                               "7"=240)

# This is alright for continuous
print(table(merged_data[,'smokers_in_house']))
print(summary(merged_data[,'smokers_in_house']))


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(merged_data[,'oily_fish_intake']))
print(summary(merged_data[,'oily_fish_intake']))
merged_data[,'oily_fish_intake']=recode(merged_data[,'oily_fish_intake'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(merged_data[,'non_oily_fish_intake']))
print(summary(merged_data[,'non_oily_fish_intake']))
merged_data[,'non_oily_fish_intake']=recode(merged_data[,'non_oily_fish_intake'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(merged_data[,'processed_meat']))
print(summary(merged_data[,'processed_meat']))
merged_data[,'processed_meat']=recode(merged_data[,'processed_meat'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(merged_data[,'poultry']))
print(summary(merged_data[,'poultry']))
merged_data[,'poultry']=recode(merged_data[,'poultry'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(merged_data[,'beef_intake']))
print(summary(merged_data[,'beef_intake']))
merged_data[,'beef_intake']=recode(merged_data[,'beef_intake'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(merged_data[,'lamb_intake']))
print(summary(merged_data[,'lamb_intake']))
merged_data[,'lamb_intake']=recode(merged_data[,'lamb_intake'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(merged_data[,'pork_intake']))
print(summary(merged_data[,'pork_intake']))
merged_data[,'pork_intake']=recode(merged_data[,'pork_intake'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(merged_data[,'cheese_intake']))
print(summary(merged_data[,'cheese_intake']))
merged_data[,'cheese_intake']=recode(merged_data[,'cheese_intake'],"0"=0,"1"=0.5,"2"=1,"3"=3,"4"=5.5,"5"=8)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(merged_data[,'salt_added_food']))
print(summary(merged_data[,'salt_added_food']))
merged_data[,'salt_added_food']=recode(merged_data[,'salt_added_food'],"1"=0,"2"=1,"3"=2,"4"=3)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(merged_data[,'varition_in_diet']))
print(summary(merged_data[,'varition_in_diet']))
merged_data[,'varition_in_diet']=recode(merged_data[,'varition_in_diet'],"1"=0,"2"=1,"3"=2)


# This is not ideal, we'll use the information from UK Biobank to recode
print(table(merged_data[,'Alc_intake_freq']))
print(summary(merged_data[,'Alc_intake_freq']))
merged_data[,'Alc_intake_freq']=recode(merged_data[,'Alc_intake_freq'],"1"=28,"2"=14,"3"=6,"4"=2,"5"=1,"6"=0)


# Let's say this is OK! 
print(table(merged_data[,'seated_box_height']))
print(summary(merged_data[,'seated_box_height']))

####################################################################################################
# PREPARING MULTI-MORBID DATA AND SCALING
####################################################################################################


#subset multi morbid rows
multi_morbid = merged_data[which(as.numeric(as.character(merged_data$no_chronic))>1),]


multi_morbid=multi_morbid[sample(1:nrow(multi_morbid),size=floor(nrow(multi_morbid)*(1/3))),]

for (k in 1:ncol(multi_morbid)) {
  if (class(multi_morbid[,k])!="factor" & k!=1) {
    multi_morbid[,k]=scale(multi_morbid[,k])
  }
}


saveRDS(multi_morbid,"../data/processed/multi_morbid_ordinal_continuous_subset.rds")