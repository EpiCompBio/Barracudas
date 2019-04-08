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

using("kamila","FactoMineR")

setwd("/Users/abieast/Documents/Imperial/Translational_Data_Science/Project/Barracudas_repo")

#Loading self made mixed data
example_mixed_data_clustering_1=readRDS("open_data/example_mixed_data_clustering_1.rds")
summary(example_mixed_data_clustering_1)

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
example_mixed_data_clustering_kamila=readRDS("open_data/example_mixed_data_clustering_kamila.rds")


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
# Kamila algorithm
################################################################################

kamila_example_mixed_data_clustering_1 <- kamila(example_mixed_data_clustering_1[,1:3],
                                                 example_mixed_data_clustering_1[,4:7], numClust = 3, numInit = 10)

print(table(as.factor(kamila_example_mixed_data_clustering_1$finalMemb),true_clusters_example_mixed_data_clustering_1))

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
# Kamila algorithm
################################################################################

kamila_example_mixed_data_clustering_kamila <- kamila(example_mixed_data_clustering_kamila[,1:4],
                                                      example_mixed_data_clustering_kamila[,5:7], numClust = 2, numInit = 10)
print(table(as.factor(kamila_example_mixed_data_clustering_kamila$finalMemb),true_clusters_kamila))
