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

using("FactoMineR")

################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

file_path<-dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(file_path)

source("C:/Users/JOE/Documents/R_utility_and_self_implementations/FAMD_plots_utility.R")
source("C:/Users/JOE/Documents/R_utility_and_self_implementations/colors_themes_utility.R")

################################################################################
# LOADING DATA 1
################################################################################

example_mixed_data_clustering_1=readRDS("../../open_data/example_mixed_data_clustering_1.rds")

for (k in 1:3) {
  example_mixed_data_clustering_1[,k]=as.numeric(as.character(example_mixed_data_clustering_1[,k]))
}

for (k in 4:ncol(example_mixed_data_clustering_1)) {
  example_mixed_data_clustering_1[,k]=as.factor(example_mixed_data_clustering_1[,k])
}

print(class(example_mixed_data_clustering_1[,1]))
print(class(example_mixed_data_clustering_1[,4]))

################################################################################
# FAMD DATA 1
################################################################################


FAMD_res_example_mixed_data_clustering_1=FAMD(example_mixed_data_clustering_1, ncp = 5, graph = FALSE)

################################################################################
# FAMD PLOT DATA 1
################################################################################


FAMD_example_mixed_data_clustering_1_ind_plot=make_FAMD_ind_plot(FAMD_res_example_mixed_data_clustering_1,dims=c(1,2),custom_theme=theme_jh)

x11()
print(FAMD_example_mixed_data_clustering_1_ind_plot)



FAMD_example_mixed_data_clustering_1_var_plot=make_FAMD_variable_graph(FAMD_res_example_mixed_data_clustering_1,dims=c(1,2),
                                                                       custom_theme=theme_jh,color_scale = distinct_scale[2])

x11(width=10,height=10)
print(FAMD_example_mixed_data_clustering_1_var_plot)


FAMD_example_mixed_data_clustering_1_cat_var_plot=make_FAMD_cat_variable_graph(FAMD_res_example_mixed_data_clustering_1,dims=c(1,2),
                                                                               custom_theme=theme_jh,color_scale = distinct_scale[2])

x11(width=10,height=10)
print(FAMD_example_mixed_data_clustering_1_cat_var_plot)

################################################################################
# LOADING DATA 2
################################################################################

example_mixed_data_clustering_2=readRDS("../../open_data/example_mixed_data_clustering_2.rds")

for (k in 1:3) {
  example_mixed_data_clustering_2[,k]=as.numeric(as.character(example_mixed_data_clustering_2[,k]))
}

for (k in 4:ncol(example_mixed_data_clustering_2)) {
  example_mixed_data_clustering_2[,k]=as.factor(example_mixed_data_clustering_2[,k])
}

################################################################################
# FAMD DATA 2
################################################################################

FAMD_res_example_mixed_data_clustering_2=FAMD(example_mixed_data_clustering_2, ncp = 5, graph = FALSE)

################################################################################
# FAMD PLOT DATA 2
################################################################################

FAMD_example_mixed_data_clustering_2_ind_plot=make_FAMD_ind_plot(FAMD_res_example_mixed_data_clustering_2,dims=c(1,2),
                                                                 custom_theme=theme_jh,color_scale = distinct_scale[2])

x11(width=10,height=10)
print(FAMD_example_mixed_data_clustering_2_ind_plot)


FAMD_example_mixed_data_clustering_2_var_plot=make_FAMD_variable_graph(FAMD_res_example_mixed_data_clustering_2,dims=c(1,2),
                         custom_theme=theme_jh,color_scale = distinct_scale[2])

x11(width=10,height=10)
print(FAMD_example_mixed_data_clustering_2_var_plot)



FAMD_example_mixed_data_clustering_2_cat_var_plot=make_FAMD_cat_variable_graph(FAMD_res_example_mixed_data_clustering_2,dims=c(1,2),
                                                                               custom_theme=theme_jh,color_scale = distinct_scale[2])

x11(width=10,height=10)
print(FAMD_example_mixed_data_clustering_2_cat_var_plot)
