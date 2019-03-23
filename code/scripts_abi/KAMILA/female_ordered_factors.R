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

source("Data/var_groupings_gender_stratified.R")
source("Git_Repo/code/utility_functions/FAMD_plots_utility.R")
source("Git_Repo/code/utility_functions/colors_themes_utility.R")
source("Git_Repo/code/utility_functions/clustering_utility.R")

################################################################################
# FAMD on the multi-morbid individuals
################################################################################

multi_morbid_female <- readRDS("Data/gender_stratified/multi_morbid_female_of.rds")
multi_morbid_female <- multi_morbid_female[,-70]
FAMD_kamila_cluster=FAMD(multi_morbid_female, ncp = ncol(multi_morbid_female), graph = FALSE)

################################################################################
# Choosing number of clusters for Kamila algorithm
################################################################################

kamRes <- kamila(multi_morbid_female[,1:35], multi_morbid_female[,36:70], numClust = 2:8, numInit = 10,
                 calcNumClust = "ps",numPredStrCvRun = 10, predStrThresh = 0.5)

kamila_cluster_choice <- plot(2:8, kamRes$nClust$psValues,
                              pch = 19, frame = FALSE, 
                              xlab="Number of clusters",
                              ylab="Prediction Strength", xlim = c(2, 8), ylim = c(0, 1))

pdf("Git_Repo/code/results_abi/KAMILA_ordered_factors/Female/female_kamila_of_cluster_choice.pdf")
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
kamila_cluster_3 <- kamila(multi_morbid_female[,1:35], multi_morbid_female[,36:70], numClust = 3, numInit = 10)

kamila_cluster_plot_3=make_FAMD_ind_plot_classes(FAMD_kamila_cluster,
                                                 classes=as.factor(kamila_cluster_3$finalMemb),dims=c(1,2),
                                                 custom_theme=theme_jh,color_scale=distinct_scale)

table(kamila_cluster_3$finalMemb)

pdf("Git_Repo/code/results_abi/KAMILA_ordered_factors/Female/female_kamila_of_cluster_plot_3.pdf")
kamila_cluster_plot_3
dev.off()

# k=2
set.seed(1)
kamila_cluster_2 <- kamila(multi_morbid_female[,1:35], multi_morbid_female[,36:69], numClust = 2, numInit = 10)

kamila_cluster_plot_2=make_FAMD_ind_plot_classes(FAMD_kamila_cluster,
                                                 classes=as.factor(kamila_cluster_2$finalMemb),dims=c(1,2),
                                                 custom_theme=theme_jh,color_scale=distinct_scale)

table(kamila_cluster_2$finalMemb)

pdf("Git_Repo/code/results_abi/KAMILA_ordered_factors/Female/female_kamila_of_cluster_plot_2.pdf")
kamila_cluster_plot_2
dev.off()

################################################
# Split categorical and continuous variables
################################################

cat_variables_female=colnames(multi_morbid_female)[sapply(sapply(multi_morbid_female,class),function(x) {x[[1]]}) == "factor" |
                                       sapply(sapply(multi_morbid_female,class),function(x) {x[[1]]}) == "ordered"]
cont_variables_female=colnames(multi_morbid_female)[sapply(multi_morbid_female,class) == "numeric"]

################################################
# Means continuous variables by cluster
################################################

KAMILA_of_2_mean_by_cluster_continuous_plot=mean_by_cluster_continuous(data=multi_morbid_female[,cont_variables_female],
                                                                       classes=as.factor(kamila_cluster_2$finalMemb),
                                                                       color_scale=NULL,custom_theme=theme_jh,title=NULL)

pdf("Git_Repo/code/results_abi/KAMILA_ordered_factors/Female/female_kamila_of_multi_morbid_mean_by_cluster_continuous_plot_2.pdf",width=10,height=10)
print(KAMILA_of_2_mean_by_cluster_continuous_plot)
dev.off()

KAMILA_of_3_mean_by_cluster_continuous_plot=mean_by_cluster_continuous(data=multi_morbid_female[,cont_variables_female],
                                                                       classes=as.factor(kamila_cluster_3$finalMemb),
                                                                       color_scale=NULL,custom_theme=theme_jh,title=NULL)


pdf("Git_Repo/code/results_abi/KAMILA_ordered_factors/Female/female_kamila_multi_morbid_mean_by_cluster_continuous_plot_3.pdf",width=10,height=10)
print(KAMILA_of_3_mean_by_cluster_continuous_plot)
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

cat_variables_split_female=splitIndices(nx=length(cat_variables_female), ncl=ceiling(length(cat_variables_female) / 9))

for (k in 1:length(cat_variables_split_female)) {
  
  
  kamila_of_2_cat_distribution_by_cluster=cat_distribution_by_cluster(data=multi_morbid_female[,cat_variables_female[cat_variables_split_female[[k]]]],
                                                                      classes=as.factor(kamila_cluster_2$finalMemb),layout=c(3,3),
                                                                      color_scale=NULL,custom_theme=theme_jh,
                                                                      title=paste0("Distributions of categorical variables by classes (",
                                                                                   k,"/",length(cat_variables_split_male),")"))
  
  
  pdf(paste0("Git_Repo/code/results_abi/KAMILA_ordered_factors/Female/female_kamila_of_2_cat_distributions_",k,"_",length(cat_variables_split_female),".pdf"),
      width=10,height=10)
  grid.draw(kamila_of_2_cat_distribution_by_cluster)
  dev.off()
}

for (k in 1:length(cat_variables_split_female)) {
  
  
  kamila_of_3_cat_distribution_by_cluster=cat_distribution_by_cluster(data=multi_morbid_female[,cat_variables_female[cat_variables_split_female[[k]]]],
                                                                      classes=as.factor(kamila_cluster_3$finalMemb),layout=c(3,3),
                                                                      color_scale=NULL,custom_theme=theme_jh,
                                                                      title=paste0("Distributions of categorical variables by classes (",
                                                                                   k,"/",length(cat_variables_split_female),")"))
  
  
  pdf(paste0("Git_Repo/code/results_abi/KAMILA_ordered_factors/Female/female_kamila_of_3_cat_distributions_",k,"_",length(cat_variables_split_female),".pdf"),
      width=10,height=10)
  grid.draw(kamila_of_3_cat_distribution_by_cluster)
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

cont_variables_split_female=splitIndices(nx=length(cont_variables_female), ncl=ceiling(length(cont_variables_female) / 9))


for (k in 1:length(cont_variables_split_female)) {
  
  kamila_of_2_cont_distribution_by_cluster=cont_distribution_by_cluster(data=multi_morbid_female[,cont_variables_female[cont_variables_split_female[[k]]]],
                                                                        classes=as.factor(kamila_cluster_2$finalMemb),layout=c(3,3),
                                                                        color_scale=NULL,custom_theme=theme_jh,
                                                                        title=paste0("Distributions of continuous variables by classes (",
                                                                                     k,"/",length(cont_variables_split_female),")"))
  
  pdf(paste0("Git_Repo/code/results_abi/KAMILA_ordered_factors/Female/female_kamila_of_2_cont_distributions_",k,"_",length(cont_variables_split_female),".pdf"),
      width=10,height=10)
  grid.draw(kamila_of_2_cont_distribution_by_cluster)
  dev.off()
}

for (k in 1:length(cont_variables_split_female)) {
  
  kamila_of_3_cont_distribution_by_cluster=cont_distribution_by_cluster(data=multi_morbid_female[,cont_variables_female[cont_variables_split_female[[k]]]],
                                                                        classes=as.factor(kamila_cluster_3$finalMemb),layout=c(3,3),
                                                                        color_scale=NULL,custom_theme=theme_jh,
                                                                        title=paste0("Distributions of continuous variables by classes (",
                                                                                     k,"/",length(cont_variables_split_female),")"))
  
  pdf(paste0("Git_Repo/code/results_abi/KAMILA_ordered_factors/Female/female_kamila_3_cont_distributions_",k,"_",length(cont_variables_split_female),".pdf"),
      width=10,height=10)
  grid.draw(kamila_of_3_cont_distribution_by_cluster)
  dev.off()
}

#########################################################
# Silhouette Plot
#########################################################  

library(cluster)

mm_unscaled <- readRDS("Data/multi_morbid_unscaled_ordinal_factors.rds")
mm_unscaled_female <- mm_unscaled[mm_unscaled$Sex == "Female",]

sil_plot_of_2 = silhouette_plot_ggplot2(data = mm_unscaled_female, classes = kamila_cluster_2$finalMemb)

pdf("Git_Repo/code/results_abi/KAMILA_ordered_factors/Female/female_silhoutte_of_2_kamila.pdf")
sil_plot_of_2
dev.off()


sil_plot_of_3 = silhouette_plot_ggplot2(data = mm_unscaled_female, classes = kamila_cluster_3$finalMemb)

pdf("Git_Repo/code/results_abi/KAMILA_ordered_factors/Female/female_silhoutte_of_3_kamila.pdf")
sil_plot_of_3
dev.off()

################################################
# Define groupings
################################################

grouping_names=list(Demographics=Demographics,BMI_related=BMI_related,
                    Activity=Activity,Vital_signs=Vital_signs,Tobacco=Tobacco,
                    Alcohol=Alcohol,Dietary=Dietary,Med_surg_hx=Med_surg_hx)

################################################
# Distribution tests
################################################

##### K=2 #####

distribution_test_df=data.frame(matrix(0,ncol=3,nrow=length(c(cont_variables_female,cat_variables_female))))
colnames(distribution_test_df)=c("var_name","Type","p_value")


distribution_test_df[,1]=c(cont_variables_female,cat_variables_female)
distribution_test_df[,2]=c(rep("Cont",length(cont_variables_female)),rep("Cat",length(cat_variables_female)))


for (k in 1:nrow(distribution_test_df)) {
  
  if (distribution_test_df[k,2]=="Cont") {
    
    anova_res=summary(lm(outcome ~ clusters,
                         data=data.frame(outcome=multi_morbid_female[,distribution_test_df[k,1]],clusters=as.factor(kamila_cluster_2$finalMemb))))
    distribution_test_df[k,3]=df(anova_res$fstatistic[1], anova_res$fstatistic[2], anova_res$fstatistic[3])
    
  } else if (distribution_test_df[k,2]=="Cat") {
    
    distribution_test_df[k,3]=chisq.test(multi_morbid_female[,distribution_test_df[k,1]],as.factor(kamila_cluster_2$finalMemb))$p.value
    
  }
  
}

distribution_test_df[,3]=p.adjust(distribution_test_df[,3],method="bonferroni")



distribution_test_df=distribution_test_df[match(colnames(multi_morbid_female)[1:ncol(multi_morbid_female)],distribution_test_df[,1]),]


significant_cluster_differences_by_variable_plot=make_significant_cluster_differences_by_variable_plot(distribution_test_df,
                                                                                                       grouping_names=grouping_names,
                                                                                                       color_scale=NULL,custom_theme=theme_jh,
                                                                                                       threshold=10^-50)


pdf(paste0("Git_Repo/code/results_abi/KAMILA_ordered_factors/Female/female_kamila_ordinal_factors_multi_morbid_cluster_differences_by_variable.pdf"),
    width=10,height=10)
print(significant_cluster_differences_by_variable_plot)
dev.off()

################################################
# random Forest variable importance
################################################

##### K=2 #####

randomForest_multi_morbid=randomForest(multi_morbid_female[,1:ncol(multi_morbid_female)], y=as.factor(kamila_cluster_2$finalMemb),ntree=500)

var_importance_df=data.frame(matrix(0,ncol=2,nrow=length(c(cont_variables_female,cat_variables_female))))
colnames(var_importance_df)=c("var_name","Type")

var_importance_df[,1]=c(cont_variables_female,cat_variables_female)
var_importance_df[,2]=c(rep("Cont",length(cont_variables_female)),rep("Cat",length(cat_variables_female)))         



var_importance_df=var_importance_df[match(colnames(multi_morbid_female)[1:ncol(multi_morbid_female)],var_importance_df[,1]),]
var_importance_df$var_importance=randomForest_multi_morbid$importance


variable_importance_plot=make_variable_importance_plot(var_importance_df,grouping_names=grouping_names, color_scale=NULL,custom_theme=theme_jh,
                                                       threshold=50)


pdf(paste0("Git_Repo/code/results_abi/KAMILA_ordered_factors/Female/female_kamila_2_ordinal_factors_multi_morbid_variable_importance.pdf"),
    width=10,height=10)
print(variable_importance_plot)
dev.off()

