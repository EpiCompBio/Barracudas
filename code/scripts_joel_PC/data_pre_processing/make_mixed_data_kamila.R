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

################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


################################################################################
# MAKING THE DATA 
################################################################################

set.seed(1)

example_mixed_data_clustering_kamila <- genMixedData(1000, 4, 3, nCatLevels=2, nConWithErr=3, nCatWithErr=2,
                    popProportions=c(0.4,0.6), conErrLev=0.5, catErrLev=0.3)


example_mixed_data_clustering_kamila=data.frame(example_mixed_data_clustering_kamila$conVars,example_mixed_data_clustering_kamila$catVars,
                                                example_mixed_data_clustering_kamila$trueID)
colnames(example_mixed_data_clustering_kamila)=c("Cont1","Cont2","Cont3","Cont4","Cat1","Cat2","Cat3","cluster")


example_mixed_data_clustering_kamila[,5:7]=apply(example_mixed_data_clustering_kamila[,5:7],2,function(x) {as.factor(x)})


saveRDS(example_mixed_data_clustering_kamila,"../data/processed_example_clustering/open_data/example_mixed_data_clustering_kamila.rds")



