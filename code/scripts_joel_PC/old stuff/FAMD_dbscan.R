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

using("FactoMineR","dbscan")

################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

file_path<-dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(file_path)

source("C:/Users/JOE/Documents/R_utility_and_self_implementations/FAMD_plots_utility.R")
source("C:/Users/JOE/Documents/R_utility_and_self_implementations/colors_themes_utility.R")

################################################################################
# LOADING DATA 
################################################################################

#Loading self made mixed data
example_mixed_data_clustering_1=readRDS("../../open_data/example_mixed_data_clustering_1.rds")

for (k in 1:3) {
  example_mixed_data_clustering_1[,k]=as.numeric(as.character(example_mixed_data_clustering_1[,k]))
}
for (k in 4:ncol(example_mixed_data_clustering_1)) {
  example_mixed_data_clustering_1[,k]=as.factor(example_mixed_data_clustering_1[,k])
}
print(class(example_mixed_data_clustering_1[,1]))
print(class(example_mixed_data_clustering_1[,4]))

true_clusters_example_mixed_data_clustering_1=c(rep(1,1000),rep(2,1000),rep(3,1000))


#Loading the mixed dat from the kamila package
example_mixed_data_clustering_kamila=readRDS("../../open_data/example_mixed_data_clustering_kamila.rds")


for (k in 5:ncol(example_mixed_data_clustering_kamila)) {
  example_mixed_data_clustering_kamila[,k]=as.factor(example_mixed_data_clustering_kamila[,k])
}

true_clusters_kamila=example_mixed_data_clustering_kamila[,ncol(example_mixed_data_clustering_kamila)]
example_mixed_data_clustering_kamila=example_mixed_data_clustering_kamila[,-ncol(example_mixed_data_clustering_kamila)]

################################################################################################################
################################################################################################################
# Selfmade mixed data
################################################################################################################
################################################################################################################

################################################################################
# FAMD
################################################################################

FAMD_res_example_mixed_data_clustering_1=FAMD(example_mixed_data_clustering_1, ncp = ncol(example_mixed_data_clustering_1), graph = FALSE)

################################################################################
# DBSCAN 
################################################################################


dbscan_FAMD_res_example_mixed_data_clustering_1=dbscan(FAMD_res_example_mixed_data_clustering_1$ind$coord[,1:7],eps=2.2)
print(unique(dbscan_FAMD_res_example_mixed_data_clustering_1$cluster))

clusters_dbscan_FAMD=dbscan_FAMD_res_example_mixed_data_clustering_1$cluster

dbscan_FAMD_classes_plot=make_FAMD_ind_plot_classes(FAMD_res_example_mixed_data_clustering_1,classes=clusters_dbscan_FAMD,
                                                    dims=c(1,2),custom_theme=theme_jh,color_scale=distinct_scale) +
  ggtitle("FAMD with DBSCAN clustering indviduals plot")


svg(filename="../results_joel_PC/FAMD_dbscan_self_mixed_data_classes_plot.svg",width=10,height=10)
print(dbscan_FAMD_classes_plot)
dev.off()

x11(width=10,height=10)
print(dbscan_FAMD_classes_plot)

table(true_clusters,clusters_dbscan_FAMD)


################################################################################################################
################################################################################################################
# Kamila made mixed data
################################################################################################################
################################################################################################################

################################################################################
# FAMD
################################################################################

FAMD_res_example_mixed_data_clustering_kamila=FAMD(example_mixed_data_clustering_kamila, ncp = ncol(example_mixed_data_clustering_kamila), graph = FALSE)

################################################################################
# Kmeans on the FAMD row coordinates
################################################################################

dbscan_FAMD_res_example_mixed_data_clustering_kamila=dbscan(FAMD_res_example_mixed_data_clustering_kamila$ind$coord[,1:7],eps=2.2)
print(unique(dbscan_FAMD_res_example_mixed_data_clustering_kamila$cluster))
clusters_dbscan_FAMD_kamila=dbscan_FAMD_res_example_mixed_data_clustering_kamila$cluster

dbscan_FAMD_kamila_classes_plot=make_FAMD_ind_plot_classes(FAMD_res_example_mixed_data_clustering_kamila,classes=clusters_dbscan_FAMD_kamila,
                                                           dims=c(1,2),
                                                           custom_theme=theme_jh,color_scale=distinct_scale)

svg(filename="../results_joel_PC/FAMD_dbscan_kamila_mixed_data_classes_plot.svg",width=10,height=10)
print(dbscan_FAMD_kamila_classes_plot)
dev.off()

x11(width=10,height=10)
print(dbscan_FAMD_kamila_classes_plot)

table(true_clusters_kamila,clusters_dbscan_FAMD_kamila)