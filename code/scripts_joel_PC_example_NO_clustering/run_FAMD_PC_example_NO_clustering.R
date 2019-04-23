
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

using("FactoMineR","ggplot2","ggrepel","viridis","RColorBrewer")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")

source("code/utility_functions/FAMD_plots_utility.R")
source("code/utility_functions/colors_themes_utility.R")



################################################################################
################################################################################
# multi-morbid individuals only
################################################################################
################################################################################


multi_morbid=readRDS("../data/processed_example_NO_clustering/example_mixed_data_NO_clustering.rds")


if(dir.exists("../results/results_joel_PC_example_NO_clustering")==FALSE) {
  dir.create("../results/results_joel_PC_example_NO_clustering")
}

################################################################################
# FAMD on the multi-morbid individuals
################################################################################

FAMD_example_NO_clustering_res=FAMD(multi_morbid,ncp=ncol(multi_morbid) +5, graph = FALSE)



#IND PLOTS
FAMD_example_NO_clustering_ind_plot_d12=make_FAMD_ind_plot(FAMD_example_NO_clustering_res,
                                                        dims=c(1,2),
                                                        custom_theme=theme_jh,color_scale=distinct_scale[2],show_labels = FALSE)



svg(filename="../results/results_joel_PC_example_NO_clustering/FAMD_example_NO_clustering_ind_plot_d12.svg",width=10,height=10)
print(FAMD_example_NO_clustering_ind_plot_d12)
dev.off()


FAMD_example_NO_clustering_ind_plot_d34=make_FAMD_ind_plot(FAMD_example_NO_clustering_res,
                                                        dims=c(3,4),
                                                        custom_theme=theme_jh,color_scale=distinct_scale[2],show_labels = FALSE)

svg(filename="../results/results_joel_PC_example_NO_clustering/FAMD_example_NO_clustering_ind_plot_d34.svg",width=10,height=10)
print(FAMD_example_NO_clustering_ind_plot_d34)
dev.off()


#VAR PLOTS
FAMD_example_NO_clustering_var_plot_d12 <- make_FAMD_variable_graph(FAMD_example_NO_clustering_res,dims=c(1,2),custom_theme=theme_jh,color_scale=distinct_scale[2])


svg(filename="../results/results_joel_PC_example_NO_clustering/FAMD_example_NO_clustering_var_plot_d12.svg",width=10,height=10)
print(FAMD_example_NO_clustering_var_plot_d12)
dev.off()


FAMD_example_NO_clustering_var_plot_d34 <- make_FAMD_variable_graph(FAMD_example_NO_clustering_res,dims=c(3,4),custom_theme=theme_jh,color_scale=distinct_scale[2])

svg(filename="../results/results_joel_PC_example_NO_clustering/FAMD_example_NO_clustering_var_plot_d34.svg",width=10,height=10)
print(FAMD_example_NO_clustering_var_plot_d34)
dev.off()


saveRDS(FAMD_example_NO_clustering_res,"../data/processed_example_NO_clustering/FAMD_example_NO_clustering_res.rds")

