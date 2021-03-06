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

# file_path<-dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(file_path)


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

multi_morbid=readRDS("../data/processed_V3_females/multi_morbid_ordinal_factors_HW_mod_female.rds")
# multi_morbid=multi_morbid[1:200,]


################################################################################
# FAMD on the multi-morbid individuals
################################################################################


#Adding a +50 to get higher explained variance
FAMD_multi_morbid_res=FAMD(multi_morbid[,15:ncol(multi_morbid)],ncp = ncol(multi_morbid)+50, graph = FALSE)


#IND PLOTS
FAMD_multi_morbid_ind_plot_d12=make_FAMD_ind_plot(FAMD_multi_morbid_res,
                   dims=c(1,2),
                   custom_theme=theme_jh,color_scale=distinct_scale[2],show_labels = FALSE)

svg(filename="../results/results_joel_HPC_V3_female/FAMD_ordinal_factors_multi_morbid_ind_plot_d12.svg",width=10,height=10)
print(FAMD_multi_morbid_ind_plot_d12)
dev.off()


FAMD_multi_morbid_ind_plot_d34=make_FAMD_ind_plot(FAMD_multi_morbid_res,
                                                  dims=c(3,4),
                                                  custom_theme=theme_jh,color_scale=distinct_scale[2],show_labels = FALSE)

svg(filename="../results/results_joel_HPC_V3_female/FAMD_ordinal_factors_multi_morbid_ind_plot_d34.svg",width=10,height=10)
print(FAMD_multi_morbid_ind_plot_d34)
dev.off()



#VAR PLOTS
FAMD_multi_morbid_var_plot_d12 <- make_FAMD_variable_graph(FAMD_multi_morbid_res,dims=c(1,2),custom_theme=theme_jh,color_scale=distinct_scale[2])


svg(filename="../results/results_joel_HPC_V3_female/FAMD_ordinal_factors_multi_morbid_var_plot_d12.svg",width=10,height=10)
print(FAMD_multi_morbid_var_plot_d12)
dev.off()


FAMD_multi_morbid_var_plot_d34 <- make_FAMD_variable_graph(FAMD_multi_morbid_res,dims=c(3,4),custom_theme=theme_jh,color_scale=distinct_scale[2])

svg(filename="../results/results_joel_HPC_V3_female/FAMD_ordinal_factors_multi_morbid_var_plot_d34.svg",width=10,height=10)
print(FAMD_multi_morbid_var_plot_d34)
dev.off()


saveRDS(FAMD_multi_morbid_res,"../data/processed_V3_females/FAMD_ordinal_factors_multi_morbid_res.rds")
