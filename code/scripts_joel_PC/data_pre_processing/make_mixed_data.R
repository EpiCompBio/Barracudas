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

using("MASS")

################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


set.seed(1)

cluster1_data=cbind(mvrnorm(n=1000, mu=c(3,2,3), Sigma=matrix(data = c(1,0.5,0.5,0.5,1,0.5,0.5,0.5,1),ncol=3,nrow=3), tol = 1e-6),
                    rbinom(n=1000,prob=.7,size=1),rbinom(n=1000,prob=.2,size=1),rbinom(n=1000,prob=.2,size=1),
                    sample(c("Category1","Category2","Category3"),1000, replace=TRUE, prob=c(0.5, 0.25, 0.25)))

cluster2_data=cbind(mvrnorm(n=1000, mu=c(-1,-1,-1), Sigma=matrix(data = c(2,-0.5,0.3,-0.5,2,-0.6,0.3,-0.6,1),ncol=3,nrow=3), tol = 1e-6),
                    rbinom(n=1000,prob=.3,size=1),rbinom(n=1000,prob=.7,size=1),rbinom(n=1000,prob=.1,size=1),
                    sample(c("Category1","Category2","Category3"),1000, replace=TRUE, prob=c(0.3, 0.4, 0.3)))

cluster3_data=cbind(mvrnorm(n=1000, mu=c(-5,1,-6), Sigma=matrix(data = c(1,0.4,-0.5,0.4,1,0.3,-0.5,0.3,1),ncol=3,nrow=3), tol = 1e-6),
                    rbinom(n=1000,prob=.2,size=1),rbinom(n=1000,prob=.3,size=1),rbinom(n=1000,prob=.9,size=1),
                    sample(c("Category1","Category2","Category3"),1000, replace=TRUE, prob=c(0.3, 0.3, 0.4)))

mixed_data=as.data.frame(rbind(cluster1_data,cluster2_data,cluster3_data))
for (k in 1:3) {
  mixed_data[,k]=as.numeric(as.character(mixed_data[,k]))
}
colnames(mixed_data)=c("Cont1","Cont2","Cont3","Binary1","Binary2","Binary3","Cat1")
print(head(mixed_data))


saveRDS(mixed_data,"../data/processed_example_clustering/example_mixed_data_clustering.rds")


