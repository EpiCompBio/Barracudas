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



#15
#5
#25
set.seed(15)


mixed_data=as.data.frame(cbind(rnorm(n=3000, mean = 0, sd = 3),
                               rnorm(n=3000, mean = 1, sd = 1),
                               rnorm(n=3000, mean = 3, sd = 2),
                               rbinom(n=3000,prob=.4,size=1),
                               rbinom(n=3000,prob=.7,size=1),
                               rbinom(n=3000,prob=.5,size=1),
                               sample(c("Category1","Category2","Category3"),3000, replace=TRUE, prob=c(0.4, 0.3, 0.3)))
)


for (k in 1:3) {
  mixed_data[,k]=as.numeric(as.character(mixed_data[,k]))
}
colnames(mixed_data)=c("Cont1","Cont2","Cont3","Binary1","Binary2","Binary3","Cat1")



saveRDS(mixed_data,"../data/processed_example_NO_clustering/example_mixed_data_NO_clustering.rds")