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
# using("FactoMineR","magrittr","reshape2","gridExtra","grid","dplyr","shadowtext")


library(FactoMineR,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(reshape2,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(magrittr,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(gridExtra,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(grid,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(dplyr,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(shadowtext,lib.loc ="/home/jheller/anaconda3/lib/R/library")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

# file_path<-dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(file_path)

# setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


# mydata=read.csv("../data/processed/UKBcompleteFeb19_subset.csv",row.names=1)
mydata=read.csv("../data/processed/UKBcompleteFeb19.csv")


source("C:/Users/JOE/Documents/R_utility_and_self_implementations/FAMD_plots_utility.R")
source("C:/Users/JOE/Documents/R_utility_and_self_implementations/colors_themes_utility.R")

# source("code/utility_functions/FAMD_plots_utility.R")
# source("code/utility_functions/colors_themes_utility.R")


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


################################################################################
################################################################################
# multi-morbid individuals only
################################################################################
################################################################################

################################################################################
# FAMD on the multi-morbid individuals
################################################################################

# FAMD_multi_morbid_res=FAMD(multi_morbid[,8:ncol(multi_morbid)], ncp = ncol(multi_morbid)-8, graph = FALSE)
# 
# 
# #IND PLOTS
# FAMD_multi_morbid_ind_plot_d12=make_FAMD_ind_plot(FAMD_multi_morbid_res,
#                    dims=c(1,2),
#                    custom_theme=theme_jh,color_scale=distinct_scale[2],show_labels = FALSE)
# 
# svg(filename="../results/results_joel_HPC/FAMD_multi_morbid_ind_plot_d12.svg",width=10,height=10)
# print(FAMD_multi_morbid_ind_plot_d12)
# dev.off()
# 
# 
# FAMD_multi_morbid_ind_plot_d34=make_FAMD_ind_plot(FAMD_multi_morbid_res,
#                                                   dims=c(3,4),
#                                                   custom_theme=theme_jh,color_scale=distinct_scale[2],show_labels = FALSE)
# 
# svg(filename="../results/results_joel_HPC/FAMD_multi_morbid_ind_plot_d34.svg",width=10,height=10)
# print(FAMD_multi_morbid_ind_plot_d34)
# dev.off()
# 
# 
# 
# #VAR PLOTS 
# FAMD_multi_morbid_var_plot_d12 <- make_FAMD_variable_graph(FAMD_multi_morbid_res,dims=c(1,2),custom_theme=theme_jh,color_scale=distinct_scale[2])
# 
# 
# svg(filename="../results/results_joel_HPC/FAMD_multi_morbid_var_plot_d12.svg",width=10,height=10)
# print(FAMD_multi_morbid_var_plot_d12)
# dev.off()
# 
# 
# FAMD_multi_morbid_var_plot_d34 <- make_FAMD_variable_graph(FAMD_multi_morbid_res,dims=c(3,4),custom_theme=theme_jh,color_scale=distinct_scale[2])
# 
# svg(filename="../results/results_joel_HPC/FAMD_multi_morbid_var_plot_d34.svg",width=10,height=10)
# print(FAMD_multi_morbid_var_plot_d34)
# dev.off()
# 
# 
# saveRDS(FAMD_multi_morbid_res,"../data/processed/FAMD_multi_morbid_res.rds")


################################################################################
################################################################################
# full dataset
################################################################################
################################################################################


################################################################################
# FAMD on the full dataset
################################################################################

FAMD_full_data_res=FAMD(mydata[,8:ncol(multi_morbid)] , ncp = ncol(multi_morbid)-8, graph = FALSE)


#IND PLOTS
FAMD_full_data_ind_plot_d12=make_FAMD_ind_plot(FAMD_full_data_res,
                                                  dims=c(1,2),
                                                  custom_theme=theme_jh,color_scale=distinct_scale[2])

svg(filename="../results/results_joel_HPC/FAMD_full_data_ind_plot_d12.svg",width=10,height=10)
print(FAMD_full_data_ind_plot_d12)
dev.off()


FAMD_full_data_ind_plot_d34=make_FAMD_ind_plot(FAMD_full_data_res,
                                               dims=c(3,4),
                                               custom_theme=theme_jh,color_scale=distinct_scale[2])

svg(filename="../results/results_joel_HPC/FAMD_full_data_ind_plot_d34.svg",width=10,height=10)
print(FAMD_full_data_ind_plot_d34)
dev.off()



#VAR PLOTS
FAMD_full_data_var_plot_d12 <- make_FAMD_variable_graph(FAMD_full_data_res,dims=c(1,2),custom_theme=theme_jh,color_scale=distinct_scale[2])


svg(filename="../results/results_joel_HPC/FAMD_full_data_var_plot_d12.svg",width=10,height=10)
print(FAMD_full_data_var_plot_d12)
dev.off()


FAMD_full_data_var_plot_d34 <- make_FAMD_variable_graph(FAMD_full_data_res,dims=c(3,4),custom_theme=theme_jh,color_scale=distinct_scale[2])

svg(filename="../results/results_joel_HPC/FAMD_full_data_var_plot_d34.svg",width=10,height=10)
print(FAMD_full_data_var_plot_d34)
dev.off()


saveRDS(FAMD_full_data_res,"../data/processed/FAMD_full_data_res.rds")

