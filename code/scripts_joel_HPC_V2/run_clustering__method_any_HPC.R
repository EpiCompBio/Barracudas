################################################################################
# LOADING LIBRARIES
################################################################################

# using<-function(...) {
#   libs<-unlist(list(...))
#   req<-unlist(lapply(libs,require,character.only=TRUE))
#   need<-libs[req==FALSE]
#   if(length(need)>0){
#     install.packages(need)
#     lapply(need,require,character.only=TRUE)
#   }
# }
# 
# using("FactoMineR","ggplot2","ggrepel","viridis","RColorBrewer","reshape2","magrittr",
#       "gridExtra","grid","dplyr","parallel","clusterCrit")



#Package from sourcing functions
library(FactoMineR,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(ggplot2,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(ggrepel,lib.loc ="/home/jheller/anaconda3/lib/R/library")


library(viridis,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(RColorBrewer, lib.loc ="/home/jheller/anaconda3/lib/R/library")


library(reshape2,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(magrittr,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(gridExtra,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(grid,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(dplyr,lib.loc ="/home/jheller/anaconda3/lib/R/library")


# Other packages used in the script
library(parallel,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(clusterCrit,lib.loc ="/home/jheller/anaconda3/lib/R/library")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

# file_path<-dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(file_path)

# setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


# multi_morbid=readRDS("../data/processed/multi_morbid_ordinal_continuous.rds")
# multi_morbid=multi_morbid[1:200,]


# source("C:/Users/JOE/Documents/R_utility_and_self_implementations/FAMD_plots_utility.R")
# source("C:/Users/JOE/Documents/R_utility_and_self_implementations/colors_themes_utility.R")
# source("C:/Users/JOE/Documents/R_utility_and_self_implementations/clustering_utility.R")



source("code/utility_functions/FAMD_plots_utility.R")
source("code/utility_functions/colors_themes_utility.R")
source("code/utility_functions/clustering_utility.R")


################################################################################
################################################################################
# multi-morbid individuals only
################################################################################
################################################################################



FAMD_multi_morbid_res=readRDS("../data/processed/FAMD_ordinal_continuous_multi_morbid_res.rds")

nb_comp_FAMD_multi_morbid=which(FAMD_multi_morbid_res$eig[,3] > 90)[1]