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

using("FactoMineR", "kamila", "tidyverse")

################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

setwd("/Users/abieast/Documents/Imperial/Translational_Data_Science/Project")

source("Git_Repo/code/utility_functions/FAMD_plots_utility.R")
source("Git_Repo/code/utility_functions/colors_themes_utility.R")
source("Git_Repo/code/utility_functions/clustering_utility.R")

################################################################################
# FAMD on the multi-morbid individuals
################################################################################

multi_morbid <- readRDS("Data/gender_stratified/multi_morbid_female_continuous.rds")
FAMD_kamila_cluster=FAMD(multi_morbid, ncp = ncol(multi_morbid), graph = FALSE)

################################################################################
# Choosing number of clusters for Kamila algorithm
################################################################################

kamRes <- kamila(multi_morbid[,1:52], multi_morbid[,53:70], numClust = 2:8, numInit = 10,
                 calcNumClust = "ps",numPredStrCvRun = 10, predStrThresh = 0.5)

kamila_cluster_choice <- plot(2:8, kamRes$nClust$psValues,
                              pch = 19, frame = FALSE, 
                              xlab="Number of clusters",
                              ylab="Prediction Strength", xlim = c(2, 8), ylim = c(0, 1))

pdf("Git_Repo/code/results_abi/KAMILA_ordinal_continuous/Female/female_kamila_oc_cluster_choice.pdf")
plot(2:8, kamRes$nClust$psValues,
     pch = 19, frame = FALSE, 
     xlab="Number of clusters",
     ylab="Prediction Strength", xlim = c(2, 8), ylim = c(0, 1))
dev.off()

################################################################################
# Kamila algorithm
################################################################################

# k=3
set.seed(1)
kamila_cluster_3 <- kamila(multi_morbid[,1:52], multi_morbid[,53:70], numClust = 3, numInit = 10)

kamila_cluster_plot_3=make_FAMD_ind_plot_classes(FAMD_kamila_cluster,
                                                 classes=as.factor(kamila_cluster_3$finalMemb),dims=c(1,2),
                                                 custom_theme=theme_jh,color_scale=distinct_scale)

table(kamila_cluster_3$finalMemb)

pdf("Git_Repo/code/results_abi/KAMILA_ordinal_continuous/Female/female_kamila_oc_cluster_plot_3.pdf")
kamila_cluster_plot_3
dev.off()

# k=2
set.seed(1)
kamila_cluster_2 <- kamila(multi_morbid[,1:52], multi_morbid[,53:70], numClust = 2, numInit = 10)

kamila_cluster_plot_2=make_FAMD_ind_plot_classes(FAMD_kamila_cluster,
                                                 classes=as.factor(kamila_cluster_2$finalMemb),dims=c(1,2),
                                                 custom_theme=theme_jh,color_scale=distinct_scale)

table(kamila_cluster_2$finalMemb)

pdf("Git_Repo/code/results_abi/KAMILA_ordinal_continuous/Female/female_kamila_oc_cluster_plot_2.pdf")
kamila_cluster_plot_2
dev.off()

################################################
# Split categorical and continuous variables
################################################

cat_variables=colnames(multi_morbid)[sapply(multi_morbid,class) == "factor"]
cont_variables=colnames(multi_morbid)[sapply(multi_morbid,class) != "factor"]

################################################
# Means continuous variables by cluster
################################################

KAMILA_oc_2_mean_by_cluster_continuous_plot=mean_by_cluster_continuous(data=multi_morbid[,cont_variables],
                                                                       classes=as.factor(kamila_cluster_2$finalMemb),
                                                                       color_scale=NULL,custom_theme=theme_jh,title=NULL)

pdf("Git_Repo/code/results_abi/KAMILA_ordinal_continuous/Female/female_kamila_oc_multi_morbid_mean_by_cluster_continuous_plot_2.pdf",width=10,height=10)
print(KAMILA_oc_2_mean_by_cluster_continuous_plot)
dev.off()

KAMILA_oc_3_mean_by_cluster_continuous_plot=mean_by_cluster_continuous(data=multi_morbid[,cont_variables],
                                                                       classes=as.factor(kamila_cluster_3$finalMemb),
                                                                       color_scale=NULL,custom_theme=theme_jh,title=NULL)


pdf("Git_Repo/code/results_abi/KAMILA_ordinal_continuous/Female/female_kamila_oc_multi_morbid_mean_by_cluster_continuous_plot_3.pdf",width=10,height=10)
print(KAMILA_oc_3_mean_by_cluster_continuous_plot)
dev.off()

#########################################################
# Distribution of continuous/categorical vars by cluster
#########################################################

library(parallel)

cat_distribution_by_cluster=function(data,classes,layout=NULL,color_scale=NULL,custom_theme=NULL,title=NULL) {
  
  if (is.null(layout)) {
    layout=c(ceiling(sqrt(ncol(data))),ceiling(sqrt(ncol(data))))
  }
  
  plot_list=list()
  
  for (k in 1:ncol(data)) {
    
    
    tmp_plot_data=as.data.frame(prop.table(table(data.frame(data[,k,drop=FALSE], classes)),margin =2))
    
    tmp_plot_data$n_ind=as.data.frame(table(data.frame(data[,k,drop=FALSE], classes)))[,3]
    tmp_plot_data=tmp_plot_data %>%
      arrange(.[[2]], desc(.[[1]]))
    tmp_plot_data$position=as.data.frame((tmp_plot_data %>% group_by(classes) %>% dplyr::mutate(cumsum(Freq))))[,5]
    tmp_plot_data$position=tmp_plot_data$position - tmp_plot_data$Freq/2
    
    tmp_plot=ggplot(data=tmp_plot_data) +
      geom_bar(aes_string(x="classes",y="Freq",fill=paste0(colnames(data)[k])),stat="identity",position="stack") +
      xlab("") + ylab("") + custom_theme + geom_text(aes(x=classes,y=position,label=n_ind),check_overlap = TRUE,
                                                     size = 4,color="black")
    
    
    
    if(!is.null(color_scale)) {
      tmp_plot = tmp_plot + scale_fill_manual(values=color_scale)
    }
    
    
    plot_list[[k]]=tmp_plot
  }
  
  
  final_plot=grid.arrange(gTree(children = gList(rectGrob(gp=gpar(fill="white",col="white", lwd=0)),
                                                 arrangeGrob(arrangeGrob(grobs=plot_list, ncol=layout[1], nrow=layout[2]),
                                                             top = textGrob(paste0(title),
                                                                            gp=gpar(font=2,fontsize=15)),
                                                             left = textGrob(paste0("Proportion"),
                                                                             gp=gpar(font=2,fontsize=12),rot = 90),
                                                             bottom = textGrob(paste0("Classes"),
                                                                               gp=gpar(font=2,fontsize=12),rot = 0)
                                                 ))))
  
  return(final_plot)
  
}

cat_variables_split=splitIndices(nx=length(cat_variables), ncl=ceiling(length(cat_variables) / 9))

for (k in 1:length(cat_variables_split)) {
  
  
  kamila_oc_2_cat_distribution_by_cluster=cat_distribution_by_cluster(data=multi_morbid[,cat_variables[cat_variables_split[[k]]]],
                                                                      classes=as.factor(kamila_cluster_2$finalMemb),layout=c(3,3),
                                                                      color_scale=NULL,custom_theme=theme_jh,
                                                                      title=paste0("Distributions of categorical variables by classes (",
                                                                                   k,"/",length(cat_variables_split),")"))
  
  
  pdf(paste0("Git_Repo/code/results_abi/KAMILA_ordinal_continuous/Female/female_kamila_oc_2_cat_distributions_",k,"_",length(cat_variables_split),".pdf"),
      width=10,height=10)
  grid.draw(kamila_oc_2_cat_distribution_by_cluster)
  dev.off()
}

for (k in 1:length(cat_variables_split)) {
  
  
  kamila_oc_3_cat_distribution_by_cluster=cat_distribution_by_cluster(data=multi_morbid[,cat_variables[cat_variables_split[[k]]]],
                                                                      classes=as.factor(kamila_cluster_3$finalMemb),layout=c(3,3),
                                                                      color_scale=NULL,custom_theme=theme_jh,
                                                                      title=paste0("Distributions of categorical variables by classes (",
                                                                                   k,"/",length(cat_variables_split),")"))
  
  
  pdf(paste0("Git_Repo/code/results_abi/KAMILA_ordinal_continuous/Female/female_kamila_oc_3_cat_distributions_",k,"_",length(cat_variables_split),".pdf"),
      width=10,height=10)
  grid.draw(kamila_oc_3_cat_distribution_by_cluster)
  dev.off()
  
}

#########################################################
# Distribution of continuous vars by cluster
#########################################################  

cont_distribution_by_cluster=function(data,classes,layout=NULL,color_scale=NULL,custom_theme=NULL,title=NULL) {
  
  if (is.null(layout)) {
    layout=c(ceiling(sqrt(ncol(data))),ceiling(sqrt(ncol(data))))
  }
  
  plot_list=list()
  
  for (k in 1:ncol(data)) {
    
    
    density_list_raw=by(as.numeric(data[,k]),INDICES=as.numeric(as.character(classes)),density,simplify = TRUE)
    
    classes_vector=NULL
    classes_list=lapply(density_list_raw,function(x) {length(x[[1]])})
    
    for (i in 1:length(density_list_raw)) {
      classes_vector=c(classes_vector,rep(names(classes_list[i]), classes_list[i]))
    }
    
    tmp_plot_data=data.frame(x=unlist(lapply(density_list_raw,function(x) {x[[1]]})),
                             y=unlist(lapply(density_list_raw,function(x) {x[[2]]})),
                             classes=as.factor(classes_vector))
    
    tmp_plot=ggplot(data=tmp_plot_data) +
      geom_line(aes(x=x,y=y,color=classes),alpha=1) +
      xlab("") + ylab("") + custom_theme + ggtitle(colnames(data)[k]) + custom_theme
    if(!is.null(color_scale)) {
      tmp_plot = tmp_plot + scale_color_manual(values=distinct_scale)
    }
    
    plot_list[[k]]=tmp_plot
  }
  
  final_plot=grid.arrange(gTree(children = gList(rectGrob(gp=gpar(fill="white",col="white", lwd=0)),
                                                 arrangeGrob(grobs=plot_list, ncol=layout[1], nrow=layout[2])
  )))
  
  return(final_plot)
  
}

cont_variables_split=splitIndices(nx=length(cont_variables), ncl=ceiling(length(cont_variables) / 9))


for (k in 1:length(cont_variables_split)) {
  
  kamila_oc_2_cont_distribution_by_cluster=cont_distribution_by_cluster(data=multi_morbid[,cont_variables[cont_variables_split[[k]]]],
                                                                        classes=as.factor(kamila_cluster_2$finalMemb),layout=c(3,3),
                                                                        color_scale=NULL,custom_theme=theme_jh,
                                                                        title=paste0("Distributions of continuous variables by classes (",
                                                                                     k,"/",length(cont_variables_split),")"))
  
  pdf(paste0("Git_Repo/code/results_abi/KAMILA_ordinal_continuous/Female/female_kamila_oc_2_cont_distributions_",k,"_",length(cont_variables_split),".pdf"),
      width=10,height=10)
  grid.draw(kamila_oc_2_cont_distribution_by_cluster)
  dev.off()
}

for (k in 1:length(cont_variables_split)) {
  
  kamila_oc_3_cont_distribution_by_cluster=cont_distribution_by_cluster(data=multi_morbid[,cont_variables[cont_variables_split[[k]]]],
                                                                        classes=as.factor(kamila_cluster_3$finalMemb),layout=c(3,3),
                                                                        color_scale=NULL,custom_theme=theme_jh,
                                                                        title=paste0("Distributions of continuous variables by classes (",
                                                                                     k,"/",length(cont_variables_split),")"))
  
  pdf(paste0("Git_Repo/code/results_abi/KAMILA_ordinal_continuous/Female/female_kamila_oc_3_cont_distributions_",k,"_",length(cont_variables_split),".pdf"),
      width=10,height=10)
  grid.draw(kamila_oc_3_cont_distribution_by_cluster)
  dev.off()
}

#########################################################
# Silhouette Plot
#########################################################  

library(cluster)

mm_unscaled <- readRDS("Data/multi_morbid_unscaled_ordinal_continuous.rds")
mm_unscaled <- mm_unscaled[mm_unscaled$Sex == "Female", ]
sil_plot_oc_2 = silhouette_plot_ggplot2(data = mm_unscaled, classes = kamila_cluster_2$finalMemb)

pdf("Git_Repo/code/results_abi/KAMILA_ordinal_continuous/Female/female_silhoutte_oc_2_kamila.pdf")
sil_plot_oc_2
dev.off()


sil_plot_oc_3 = silhouette_plot_ggplot2(data = mm_unscaled, classes = kamila_cluster_3$finalMemb)

pdf("Git_Repo/code/results_abi/KAMILA_ordinal_continuous/Female/female_silhoutte_oc_3_kamila.pdf")
sil_plot_oc_3
dev.off()

