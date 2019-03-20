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

mydata=read.csv("Data/UKBcompleteFeb19.csv")

source("Git_Repo/code/utility_functions/FAMD_plots_utility.R")
source("Git_Repo/code/utility_functions/colors_themes_utility.R")
source("Git_Repo/code/utility_functions/clustering_utility.R")


################################################################################
# PRE-PROCESSING
################################################################################

#define obese BMI > 35
mydata$obese = ifelse(mydata$BMI >= 35, 1, 0)

#define outcome cols
outcomes = c('diabetes','mi','stroke','angina','obese')

outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(mydata))

#col of chronic diseases
mydata$no_chronic = apply(mydata[,outcome_cols],1,sum)

#change gender levels and remove gender that is not used anymore
mydata$Sex = factor(ifelse(mydata$gender == 0, 'Female','Male'))
mydata$gender=NULL

#binary cols
binary_cols = which(unlist(sapply(mydata, function(x) length(levels(factor(x)))==2)))
mydata[,binary_cols]=lapply(mydata[,binary_cols],as.factor)

#re-organize columns
mydata=mydata %>% select(eid,mi,angina,stroke,htn,obese,no_chronic, everything())

#subset multi morbid rows
multi_morbid = mydata[which(mydata$no_chronic>1),]

#remove asthma variable 
multi_morbid$dvt_asthma_copd_atopy <- NULL

#Set eid as rownames
rownames(multi_morbid) <- multi_morbid[,1]
multi_morbid[,1] <- NULL

mydata[,'no_chronic']=as.factor(mydata[,'no_chronic'])
multi_morbid[,'no_chronic']=as.factor(multi_morbid[,'no_chronic'])

saveRDS(multi_morbid, file = "Data/mm_unscaled.rds")

for (k in 1:ncol(mydata)) {
  if (class(mydata[,k])!="factor") {
    mydata[,k]=scale(mydata[,k])
  }
}

for (k in 1:ncol(multi_morbid)) {
  if (class(multi_morbid[,k])!="factor") {
    multi_morbid[,k]=scale(multi_morbid[,k])
  }
}

saveRDS(multi_morbid, file = "Data/mm_scaled.rds")

################################################################################
################################################################################
# multi-morbid individuals only
################################################################################
################################################################################

################################################################################
# FAMD on the multi-morbid individuals
################################################################################

multi_morbid <- readRDS("Data/mm_scaled.rds")
multi_morbid <- multi_morbid[ ,-c(1:6)]

FAMD_kamila_cluster=FAMD(multi_morbid, ncp = ncol(multi_morbid), graph = FALSE)

################################################################################
# Choosing number of clusters for Kamila algorithm
################################################################################

kamRes <- kamila(multi_morbid[,1:53], multi_morbid[,54:71], numClust = 2:8, numInit = 10,
                 calcNumClust = "ps",numPredStrCvRun = 10, predStrThresh = 0.5)

kamila_cluster_choice <- plot(2:8, kamRes$nClust$psValues,
     pch = 19, frame = FALSE, 
     xlab="Number of clusters",
     ylab="Prediction Strength", xlim = c(2, 8), ylim = c(0, 1))

svg("Git_Repo/code/results_abi/kamila_cluster_choice.svg")
plot(2:8, kamRes$nClust$psValues,
     pch = 19, frame = FALSE, 
     xlab="Number of clusters",
     ylab="Prediction Strength", xlim = c(2, 8), ylim = c(0, 1))
dev.off()

# 2 clusters seems best although could use 3 (highest number of clusters giving prediction strength over 0.8/0.9)

################################################################################
# Kamila algorithm
################################################################################

# k=3
set.seed(1)
kamila_cluster_3 <- kamila(multi_morbid[,1:53], multi_morbid[,54:71], numClust = 3, numInit = 10)

kamila_cluster_plot_3=make_FAMD_ind_plot_classes(FAMD_kamila_cluster,
                                                          classes=as.factor(kamila_cluster_3$finalMemb),dims=c(1,2),
                                                          custom_theme=theme_jh,color_scale=distinct_scale)

table(kamila_cluster_3$finalMemb)

svg("Git_Repo/code/results_abi/KAMILA/kamila_cluster_plot_3.svg")
kamila_cluster_plot_3
dev.off()

# k=2
set.seed(1)
kamila_cluster_2 <- kamila(multi_morbid[,1:53], multi_morbid[,54:71], numClust = 2, numInit = 10)

kamila_cluster_plot_2=make_FAMD_ind_plot_classes(FAMD_kamila_cluster,
                                               classes=as.factor(kamila_cluster_2$finalMemb),dims=c(1,2),
                                               custom_theme=theme_jh,color_scale=distinct_scale)

table(kamila_cluster_2$finalMemb)

svg("Git_Repo/code/results_abi/KAMILA/kamila_cluster_plot_2.svg")
kamila_cluster_plot_2
dev.off()

################################################
# Means continuous variables by cluster
################################################

##### Function for means plot #####
data=multi_morbid[,cont_variables]
classes=as.factor(kamila_cluster_3$finalMemb)
color_scale=NULL
custom_theme=theme_jh
title=NULL

cat_variables=colnames(multi_morbid)[sapply(multi_morbid,class) == "factor"]
cont_variables=colnames(multi_morbid)[sapply(multi_morbid,class) != "factor"]

#####cont_variables=cont_variables[2:length(cont_variables)]#####

mean_by_cluster_continuous=function(data,classes,color_scale=NULL,custom_theme=NULL,title=NULL) {
  
  data_and_classes = data.frame(data, classes)
  means_clusters=aggregate(data_and_classes[,-c(ncol(data_and_classes))],
                           by=list(data_and_classes[,ncol(data_and_classes)]),mean)
  
  means_clusters=apply(means_clusters,2,as.numeric)
  
  plot_data=melt(setNames(data.frame(t(means_clusters)[-1,],id=colnames(data)),c(levels(classes),"id")),
                 id.var="id")
  
  colnames(plot_data)[2]="cluster"
  
  mean_clusters_plot=ggplot(plot_data, aes(x=id,y=value,group=cluster,colour=cluster)) +
    geom_point() + geom_line(aes(lty=cluster)) + 
    xlab("Variables") + ylab("Mean") 
  if (!is.null(color_scale)) {
    mean_clusters_plot=mean_clusters_plot + scale_colour_manual(name="Cluster", values = color_scale, labels = levels(classes)) +
      scale_linetype_discrete(name="Cluster",labels = color_scale)
  }
  if (!is.null(title)) {
    mean_clusters_plot = mean_clusters_plot + ggtitle(title)
  }
  mean_clusters_plot=mean_clusters_plot + custom_theme + 
    theme(axis.text.x = element_text(angle = 90))
  return(mean_clusters_plot)
}

KAMILA_mean_by_cluster_continuous_plot=mean_by_cluster_continuous(data=multi_morbid[,cont_variables],
                                                                  classes=as.factor(kamila_cluster_2$finalMemb),
                                                                  color_scale=NULL,custom_theme=theme_jh,title=NULL)

svg(filename="Git_Repo/code/results_abi/kamila_multi_morbid_mean_by_cluster_continuous_plot_2.svg",width=10,height=10)
print(KAMILA_mean_by_cluster_continuous_plot)
dev.off()

KAMILA_mean_by_cluster_continuous_plot=mean_by_cluster_continuous(data=multi_morbid[,cont_variables],
                                                                       classes=as.factor(kamila_cluster_3$finalMemb),
                                                                       color_scale=NULL,custom_theme=theme_jh,title=NULL)


svg(filename="Git_Repo/code/results_abi/kamila_multi_morbid_mean_by_cluster_continuous_plot_3.svg",width=10,height=10)
print(KAMILA_mean_by_cluster_continuous_plot)
dev.off()

#########################################################
# Distribution of continuous/categorical vars by cluster
#########################################################
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
  
  
  kamila_cat_distribution_by_cluster=cat_distribution_by_cluster(data=multi_morbid[,cat_variables[cat_variables_split[[k]]]],
                                                                      classes=as.factor(kamila_cluster_2$finalMemb),layout=c(3,3),
                                                                      color_scale=NULL,custom_theme=theme_jh,
                                                                      title=paste0("Distributions of categorical variables by classes (",
                                                                                   k,"/",length(cat_variables_split),")"))
  
  
  svg(filename=paste0("Git_Repo/code/results_abi/kamila_cat_distributions_",k,"_",length(cat_variables_split),".svg"),
      width=10,height=10)
  grid.draw(kamila_cat_distribution_by_cluster)
  dev.off()
}
  
for (k in 1:length(cat_variables_split)) {
    
    
    kamila_cat_distribution_by_cluster_3=cat_distribution_by_cluster(data=multi_morbid[,cat_variables[cat_variables_split[[k]]]],
                                                                   classes=as.factor(kamila_cluster_3$finalMemb),layout=c(3,3),
                                                                   color_scale=NULL,custom_theme=theme_jh,
                                                                   title=paste0("Distributions of categorical variables by classes (",
                                                                                k,"/",length(cat_variables_split),")"))
    
    
    svg(filename=paste0("Git_Repo/code/results_abi/kamila_3_cat_distributions_",k,"_",length(cat_variables_split),".svg"),
        width=10,height=10)
    grid.draw(kamila_cat_distribution_by_cluster)
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
    
    kamila_cont_distribution_by_cluster=cont_distribution_by_cluster(data=multi_morbid[,cont_variables[cont_variables_split[[k]]]],
                                                                          classes=as.factor(kamila_cluster_2$finalMemb),layout=c(3,3),
                                                                          color_scale=NULL,custom_theme=theme_jh,
                                                                          title=paste0("Distributions of continuous variables by classes (",
                                                                                       k,"/",length(cont_variables_split),")"))
    
    svg(filename=paste0("Git_Repo/code/results_abi/kamila_cont_distributions_",k,"_",length(cont_variables_split),".svg"),
        width=10,height=10)
    grid.draw(kamila_cont_distribution_by_cluster)
    dev.off()
  }

for (k in 1:length(cont_variables_split)) {
  
  kamila_cont_distribution_by_cluster=cont_distribution_by_cluster(data=multi_morbid[,cont_variables[cont_variables_split[[k]]]],
                                                                   classes=as.factor(kamila_cluster_3$finalMemb),layout=c(3,3),
                                                                   color_scale=NULL,custom_theme=theme_jh,
                                                                   title=paste0("Distributions of continuous variables by classes (",
                                                                                k,"/",length(cont_variables_split),")"))
  
  svg(filename=paste0("Git_Repo/code/results_abi/kamila_3_cont_distributions_",k,"_",length(cont_variables_split),".svg"),
      width=10,height=10)
  grid.draw(kamila_cont_distribution_by_cluster)
  dev.off()
}

#########################################################
# Heatmaps of variables
#########################################################

library(corrplot)
svg("Git_Repo/code/results_abi/KAMILA/corrplot_cont_vars.svg")
corrplot(cor(multi_morbid[,1:53]))  
dev.off()

library(pheatmap)
svg("Git_Repo/code/results_abi/KAMILA/heatmap_cont_vars.svg")
pheatmap(cor(multi_morbid[,1:53]))
dev.off()

########################################################################################################
# Plot a silhouette plot (cluster integrity)
#### INPUTS
# data : data used for clustering
# classes : clusters
# daisy_metric : metric for the daisy function to use
# custom_theme : custom ggplot theme to be applied
# color_scale : color scale for the plot
# title : title for the plot
#### OUTPUT
# ggplot object of the silhouette plot
########################################################################################################

silhouette_plot_ggplot2=function(data,classes,daisy_metric="euclidean",custom_theme=NULL,color_scale=NULL,title=NULL) {
  
  
  
  dissimilarity_matrix <-  daisy(data,metric=daisy_metric)
  silhouette_object <- silhouette(as.numeric(as.character(classes)), dissimilarity_matrix)
  attr(silhouette_object,"class")=NULL
  silhouette_data=as.data.frame(as.matrix(silhouette_object))
  silhouette_data=silhouette_data[with(silhouette_data,order(silhouette_data[,1],silhouette_data[,3],decreasing=TRUE)),]
  silhouette_data$id=1:nrow(silhouette_data)
  silhouette_data[,"cluster"]=as.factor(silhouette_data[,'cluster'])
  
  silhouette_plot = ggplot(silhouette_data) + geom_bar(aes(x=id,y=sil_width,fill=cluster),stat = "identity",position = "dodge")+
    xlab(" ") + ylab("Silhouette width") +  custom_theme
  if (!is.null(color_scale)) {
    scale_fill_manual(name="Clusters", labels=levels(silhouette_data$cluster),
                      values=color_scale)
  }
  
  return(silhouette_plot)
}

## Use unscaled data ##

mm_unscaled <- readRDS("Data/mm_unscaled.rds")
mm_unscaled <- mm_unscaled[ , -c(1:6)]

sil_plot = silhouette_plot_ggplot2(data = mm_unscaled, classes = kamila_cluster_2$finalMemb)
svg("Git_Repo/code/results_abi/KAMILA_ordered_factors/silhoutte_of_2_kamila.svg")
sil_plot
dev.off()


sil_plot = silhouette_plot_ggplot2(data = mm_unscaled, classes = kamila_cluster_3$finalMemb)
svg("Git_Repo/code/results_abi/KAMILA/silhoutte_of_3_kamila.svg")
sil_plot
dev.off()

