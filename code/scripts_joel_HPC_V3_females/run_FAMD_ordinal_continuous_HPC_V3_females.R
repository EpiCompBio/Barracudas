
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
# using("FactoMineR","ggplot2","ggrepel","viridis","RColorBrewer")

#Package from sourcing functions
library(FactoMineR,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(ggplot2,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(ggrepel,lib.loc ="/home/jheller/anaconda3/lib/R/library")


library(viridis,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(RColorBrewer, lib.loc ="/home/jheller/anaconda3/lib/R/library")

################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################


# setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


# source("C:/Users/JOE/Documents/R_utility_and_self_implementations/FAMD_plots_utility.R")
# source("C:/Users/JOE/Documents/R_utility_and_self_implementations/colors_themes_utility.R")

source("code/utility_functions/FAMD_plots_utility.R")
source("code/utility_functions/colors_themes_utility.R")



################################################################################
################################################################################
# multi-morbid individuals only
################################################################################
################################################################################


multi_morbid=readRDS("../data/processed_V3/multi_morbid_ordinal_continuous_HW_mod_female.rds")
# multi_morbid=multi_morbid[1:200,]

################################################################################
# FAMD on the multi-morbid individuals
################################################################################
²&