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

using("randomForest","caret","cluster","FactoMineR","apcluster")

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



example_mixed_data_clustering_1_sub=rbind(example_mixed_data_clustering_1[1:50,],
                                               example_mixed_data_clustering_1[1001:1050,],
                                               example_mixed_data_clustering_1[2001:2050,])

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
# Random forests for the proximty measures
################################################################################

RF_proximity_measures=randomForest(x=example_mixed_data_clustering_1)$proximity

################################################################################
# Partitioning around medoids on the proximity measure
################################################################################

pam_clustering_RF=apclusterK(s=as.SparseSimilarityMatrix(RF_proximity_measures),details=TRUE,K=3)

cl1 <- cbind(rnorm(20, 0.2, 0.05), rnorm(20, 0.8, 0.06))
cl2 <- cbind(rnorm(20, 0.7, 0.08), rnorm(20, 0.3, 0.05))
x <- rbind(cl1, cl2)

sim <- negDistMat(x, r=2)


bleh=as.DenseSimilarityMatrix(sp, fill=0)
################################################################################
# Plotting the clusters using FAMD
################################################################################

#Using our method
pam_clustering_RF_classes_plot=make_FAMD_ind_plot_classes(FAMD_res_example_mixed_data_clustering_1,
                                                          classes=as.factor(pam_clustering_RF$clustering),dims=c(1,2),
                                                          custom_theme=theme_jh,color_scale=distinct_scale)

x11()
print(pam_clustering_RF_classes_plot)

print(table(as.factor(pam_clustering_RF$clustering),true_clusters))


################################################################################################################
################################################################################################################
# kamila mixed data
################################################################################################################
################################################################################################################


################################################################################
# FAMD
################################################################################

FAMD_res_example_mixed_data_clustering_kamila=FAMD(example_mixed_data_clustering_kamila, ncp = ncol(example_mixed_data_clustering_kamila), graph = FALSE)

################################################################################
# Random forests for the proximty measures
################################################################################

RF_proximity_measures_kamila=randomForest(x=example_mixed_data_clustering_kamila)$proximity

################################################################################
# Partitioning around medoids on the proximity measure
################################################################################

pam_clustering_RF_kamila=pam(RF_proximity_measures_kamila, 2)


################################################################################
# Plotting the clusters using FAMD
################################################################################

#Using our method
pam_clustering_RF_kamila_classes_plot=make_FAMD_ind_plot_classes(FAMD_res_example_mixed_data_clustering_kamila,
                                                                 classes=as.factor(pam_clustering_RF_kamila$clustering),dims=c(1,2),
                                                                 custom_theme=theme_jh,color_scale=distinct_scale)

x11()
print(pam_clustering_RF_kamila_classes_plot)

print(table(as.factor(pam_clustering_RF_kamila$clustering),true_clusters_kamila))

