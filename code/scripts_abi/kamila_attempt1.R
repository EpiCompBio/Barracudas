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

using("FactoMineR", "kamila", "tidyverse")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

setwd("/Users/abieast/Documents/Imperial/Translational_Data_Science/Project")

mydata=read.csv("Data/UKBcompleteFeb19.csv")

source("Git_Repo/code/utility_functions/FAMD_plots_utility.R")
source("Git_Repo/code/utility_functions/colors_themes_utility.R")


################################################################################
# PRE-PROCESSING
################################################################################

#define obese BMI > 35
mydata$obese = ifelse(mydata$BMI >= 35, 1, 0)

#define outcome cols
outcomes = c('diabetes','mi','stroke','angina','obese')

outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(mydata))

#col of chronic diseases
mydata$no_chronic = apply(mydata[,outcome_cols],1,sum)

#change gender levels and remove gender that is not used anymore
mydata$Sex = factor(ifelse(mydata$gender == 0, 'Female','Male'))
mydata$gender=NULL

#binary cols
binary_cols = which(unlist(sapply(mydata, function(x) length(levels(factor(x)))==2)))
mydata[,binary_cols]=lapply(mydata[,binary_cols],as.factor)

#re-organize columns
mydata=mydata %>% select(eid,mi,angina,stroke,htn,obese,no_chronic, everything())

#subset multi morbid rows
multi_morbid = mydata[which(mydata$no_chronic>1),]

#Set eid as rownames
rownames(multi_morbid) <- multi_morbid[,1]
multi_morbid[,1] <- NULL

for (k in 1:ncol(mydata)) {
  if (class(mydata[,k])!="factor") {
    mydata[,k]=scale(mydata[,k])
  }
}

for (k in 1:ncol(multi_morbid)) {
  if (class(multi_morbid[,k])!="factor") {
    multi_morbid[,k]=scale(multi_morbid[,k])
  }
}

saveRDS(multi_morbid, file = "Data/multi_morbid_final.rds")
