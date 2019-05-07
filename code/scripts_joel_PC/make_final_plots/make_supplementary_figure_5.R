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

using("ggplot2","grid","gridExtra","ggrepel","dplyr","reshape2")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


source("C:/Users/JOE/Documents/R_utility_and_self_implementations/colors_themes_utility.R")
source("code/utility_functions/clustering_utility.R")


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

theme_jh = theme_jh + theme(plot.title=element_text(family="serif",face="bold",color="black",hjust=0.5,vjust=0.5,size=14))

#####################################################################################################################
#####################################################################################################################
# KAMILA 
#####################################################################################################################
#####################################################################################################################


################################################################################
# LOAD NECESSARY RESULTS
################################################################################


kamila_multi_morbid_female=
  readRDS("../real_results_from_HPC/results_used_for_final_figures/supplementary_figure_5/kamila_multi_morbid_female.rds")




kamila_multi_morbid_male=
  readRDS("../real_results_from_HPC/results_used_for_final_figures/supplementary_figure_5/kamila_multi_morbid_male.rds")


multi_morbid_female=readRDS("../data/processed_V3_females/multi_morbid_ordinal_factors_HW_mod_female.rds")

multi_morbid_male=readRDS("../data/processed_V3_males/multi_morbid_ordinal_factors_HW_mod_male.rds")


disease_variables=colnames(multi_morbid_female)[2:10]
disease_variables=disease_variables[-6]


data=multi_morbid_female[,disease_variables]
colnames(data)=c("CAD","Stroke","Morbidly_obese","Diabetes","Hypertension","Heart_failure","Intracranial_haemorrhage","Peripheral_vascular")
classes=kamila_multi_morbid_female$finalMemb
custom_theme=theme_jh


############################
#FEMALE
############################

plot_list=list()

for (k in 1:ncol(data)) {
  
  
  tmp_plot_data=as.data.frame(prop.table(table(data.frame(data[,k,drop=FALSE], classes)),margin =2))
  
  tmp_plot_data$n_ind=as.data.frame(table(data.frame(data[,k,drop=FALSE], classes)))[,3]
  tmp_plot_data=tmp_plot_data %>%
    arrange(.[[2]], desc(.[[1]]))
  tmp_plot_data$position=as.data.frame((tmp_plot_data %>% group_by(classes) %>% dplyr::mutate(cumsum(Freq))))[,5]
  tmp_plot_data$position=tmp_plot_data$position - tmp_plot_data$Freq/2
  
  
  if (k==1) {
    
  tmp_plot=ggplot(data=tmp_plot_data) +
    geom_bar(aes_string(x="classes",y="Freq",fill=paste0(colnames(data)[k])),stat="identity",position="stack") +
    xlab("") + ylab("") + custom_theme + geom_text(aes(x=classes,y=position,label=n_ind),check_overlap = TRUE,
                                                   size = 4,color="black") + ggtitle(colnames(data)[k]) +
    scale_fill_manual("Diagnosis of disease of interest", label=c("No","Yes"),values=gg_color_hue(2),
                      breaks=levels(tmp_plot_data$CAD))
  
  
    legend <- get_legend( tmp_plot)
    tmp_plot <-  tmp_plot + theme(legend.position="none")
    
  } else {
    
    tmp_plot=ggplot(data=tmp_plot_data) +
      geom_bar(aes_string(x="classes",y="Freq",fill=paste0(colnames(data)[k])),stat="identity",position="stack") +
      xlab("") + ylab("") + custom_theme + geom_text(aes(x=classes,y=position,label=n_ind),check_overlap = TRUE,
                                                     size = 4,color="black") + ggtitle(colnames(data)[k]) +
      theme(legend.position="none")
    
  }
  
  plot_list[[k]]=tmp_plot
}


plot_list=c(plot_list,list(legend))

females_plot=gTree(children = gList(rectGrob(gp=gpar(fill="white",col="white", lwd=0)),
                                               arrangeGrob(arrangeGrob(grobs=c(plot_list), ncol=3, nrow=3),
                                                           top = textGrob(paste0("Women"),
                                                                          gp=gpar(font=2,fontsize=15)))))



############################
#MALE
############################

data=multi_morbid_male[,disease_variables]
colnames(data)=c("CAD","Stroke","Morbidly_obese","Diabetes","Hypertension","Heart_failure","Intracranial_haemorrhage","Peripheral_vascular")
classes=kamila_multi_morbid_male$finalMemb
custom_theme=theme_jh


plot_list=list()

for (k in 1:ncol(data)) {
  
  
  tmp_plot_data=as.data.frame(prop.table(table(data.frame(data[,k,drop=FALSE], classes)),margin =2))
  
  tmp_plot_data$n_ind=as.data.frame(table(data.frame(data[,k,drop=FALSE], classes)))[,3]
  tmp_plot_data=tmp_plot_data %>%
    arrange(.[[2]], desc(.[[1]]))
  tmp_plot_data$position=as.data.frame((tmp_plot_data %>% group_by(classes) %>% dplyr::mutate(cumsum(Freq))))[,5]
  tmp_plot_data$position=tmp_plot_data$position - tmp_plot_data$Freq/2
  
  
  if (k==1) {
    
    tmp_plot=ggplot(data=tmp_plot_data) +
      geom_bar(aes_string(x="classes",y="Freq",fill=paste0(colnames(data)[k])),stat="identity",position="stack") +
      xlab("") + ylab("") + custom_theme + geom_text(aes(x=classes,y=position,label=n_ind),check_overlap = TRUE,
                                                     size = 4,color="black") + ggtitle(colnames(data)[k]) +
      scale_fill_manual("Diagnosis of disease of interest", label=c("No","Yes"),values=gg_color_hue(2),
                        breaks=levels(tmp_plot_data$CAD))
    
    
    legend <- get_legend( tmp_plot)
    tmp_plot <-  tmp_plot + theme(legend.position="none")
    
  } else {
    
    tmp_plot=ggplot(data=tmp_plot_data) +
      geom_bar(aes_string(x="classes",y="Freq",fill=paste0(colnames(data)[k])),stat="identity",position="stack") +
      xlab("") + ylab("") + custom_theme + geom_text(aes(x=classes,y=position,label=n_ind),check_overlap = TRUE,
                                                     size = 4,color="black") + ggtitle(colnames(data)[k]) +
      theme(legend.position="none")
    
  }
  
  plot_list[[k]]=tmp_plot
}


plot_list=c(plot_list,list(legend))


males_plot=gTree(children = gList(rectGrob(gp=gpar(fill="white",col="white", lwd=0)),
                                    arrangeGrob(arrangeGrob(grobs=c(plot_list), ncol=3, nrow=3),
                                                top = textGrob(paste0("Men"),
                                                               gp=gpar(font=2,fontsize=16)))))





final_plot=arrangeGrob(rectGrob(gp = gpar(col = "white")),heights=c(0.025,1),
                 arrangeGrob(grobs=list(females_plot,linesGrob(x = unit(c(0.5, 0.5), "npc"),
                                                               y = unit(c(0.1, 0.9), "npc")),males_plot),ncol=3,widths=c(1,0.01,1)),
                 top=textGrob("Kamila",
                              gp=gpar(fontsize=20, col="black",fontface="bold")))


x11()
grid.draw(final_plot)




#####################################################################################################################
#####################################################################################################################
# FAMD KMEANS 
#####################################################################################################################
#####################################################################################################################


################################################################################
# LOAD NECESSARY RESULTS
################################################################################




FAMD_kmeans_ordinal_factors_multi_morbid_female=
  readRDS("../real_results_from_HPC/results_used_for_final_figures/supplementary_figure_5/FAMD_kmeans_ordinal_factors_multi_morbid_female.rds")


FAMD_kmeans_ordinal_factors_multi_morbid_male=
  readRDS("../real_results_from_HPC/results_used_for_final_figures/supplementary_figure_5/FAMD_kmeans_ordinal_factors_multi_morbid_male.rds")


multi_morbid_female=readRDS("../data/processed_V3_females/multi_morbid_ordinal_factors_HW_mod_female.rds")

multi_morbid_male=readRDS("../data/processed_V3_males/multi_morbid_ordinal_factors_HW_mod_male.rds")


disease_variables=colnames(multi_morbid_female)[2:10]
disease_variables=disease_variables[-6]


data=multi_morbid_female[,disease_variables]
colnames(data)=c("CAD","Stroke","Morbidly_obese","Diabetes","Hypertension","Heart_failure","Intracranial_haemorrhage","Peripheral_vascular")
classes=FAMD_kmeans_ordinal_factors_multi_morbid_female$cluster
custom_theme=theme_jh


plot_list=list()

for (k in 1:ncol(data)) {
  
  
  tmp_plot_data=as.data.frame(prop.table(table(data.frame(data[,k,drop=FALSE], classes)),margin =2))
  
  tmp_plot_data$n_ind=as.data.frame(table(data.frame(data[,k,drop=FALSE], classes)))[,3]
  tmp_plot_data=tmp_plot_data %>%
    arrange(.[[2]], desc(.[[1]]))
  tmp_plot_data$position=as.data.frame((tmp_plot_data %>% group_by(classes) %>% dplyr::mutate(cumsum(Freq))))[,5]
  tmp_plot_data$position=tmp_plot_data$position - tmp_plot_data$Freq/2
  
  
  if (k==1) {
    
    tmp_plot=ggplot(data=tmp_plot_data) +
      geom_bar(aes_string(x="classes",y="Freq",fill=paste0(colnames(data)[k])),stat="identity",position="stack") +
      xlab("") + ylab("") + custom_theme + geom_text(aes(x=classes,y=position,label=n_ind),check_overlap = TRUE,
                                                     size = 4,color="black") + ggtitle(colnames(data)[k]) +
      scale_fill_manual("Diagnosis of disease of interest", label=c("No","Yes"),values=gg_color_hue(2),
                        breaks=levels(tmp_plot_data$CAD))
    
    
    legend <- get_legend( tmp_plot)
    tmp_plot <-  tmp_plot + theme(legend.position="none")
    
  } else {
    
    tmp_plot=ggplot(data=tmp_plot_data) +
      geom_bar(aes_string(x="classes",y="Freq",fill=paste0(colnames(data)[k])),stat="identity",position="stack") +
      xlab("") + ylab("") + custom_theme + geom_text(aes(x=classes,y=position,label=n_ind),check_overlap = TRUE,
                                                     size = 4,color="black") + ggtitle(colnames(data)[k]) +
      theme(legend.position="none")
    
  }
  
  plot_list[[k]]=tmp_plot
}


plot_list=c(plot_list,list(legend))

females_plot=gTree(children = gList(rectGrob(gp=gpar(fill="white",col="white", lwd=0)),
                                    arrangeGrob(arrangeGrob(grobs=c(plot_list), ncol=3, nrow=3),
                                                top = textGrob(paste0("Women"),
                                                               gp=gpar(font=2,fontsize=15)))))




data=multi_morbid_male[,disease_variables]
colnames(data)=c("CAD","Stroke","Morbidly_obese","Diabetes","Hypertension","Heart_failure","Intracranial_haemorrhage","Peripheral_vascular")
classes=FAMD_kmeans_ordinal_factors_multi_morbid_male$cluster
custom_theme=theme_jh


plot_list=list()

for (k in 1:ncol(data)) {
  
  
  tmp_plot_data=as.data.frame(prop.table(table(data.frame(data[,k,drop=FALSE], classes)),margin =2))
  
  tmp_plot_data$n_ind=as.data.frame(table(data.frame(data[,k,drop=FALSE], classes)))[,3]
  tmp_plot_data=tmp_plot_data %>%
    arrange(.[[2]], desc(.[[1]]))
  tmp_plot_data$position=as.data.frame((tmp_plot_data %>% group_by(classes) %>% dplyr::mutate(cumsum(Freq))))[,5]
  tmp_plot_data$position=tmp_plot_data$position - tmp_plot_data$Freq/2
  
  
  if (k==1) {
    
    tmp_plot=ggplot(data=tmp_plot_data) +
      geom_bar(aes_string(x="classes",y="Freq",fill=paste0(colnames(data)[k])),stat="identity",position="stack") +
      xlab("") + ylab("") + custom_theme + geom_text(aes(x=classes,y=position,label=n_ind),check_overlap = TRUE,
                                                     size = 4,color="black") + ggtitle(colnames(data)[k]) +
      scale_fill_manual("Diagnosis of disease of interest", label=c("No","Yes"),values=gg_color_hue(2),
                        breaks=levels(tmp_plot_data$CAD))
    
    
    legend <- get_legend( tmp_plot)
    tmp_plot <-  tmp_plot + theme(legend.position="none")
    
  } else {
    
    tmp_plot=ggplot(data=tmp_plot_data) +
      geom_bar(aes_string(x="classes",y="Freq",fill=paste0(colnames(data)[k])),stat="identity",position="stack") +
      xlab("") + ylab("") + custom_theme + geom_text(aes(x=classes,y=position,label=n_ind),check_overlap = TRUE,
                                                     size = 4,color="black") + ggtitle(colnames(data)[k]) +
      theme(legend.position="none")
    
  }
  
  plot_list[[k]]=tmp_plot
}


plot_list=c(plot_list,list(legend))


males_plot=gTree(children = gList(rectGrob(gp=gpar(fill="white",col="white", lwd=0)),
                                  arrangeGrob(arrangeGrob(grobs=c(plot_list), ncol=3, nrow=3),
                                              top = textGrob(paste0("Men"),
                                                             gp=gpar(font=2,fontsize=16)))))





final_plot=arrangeGrob(rectGrob(gp = gpar(col = "white")),heights=c(0.025,1),
                       arrangeGrob(grobs=list(females_plot,linesGrob(x = unit(c(0.5, 0.5), "npc"),
                                                                     y = unit(c(0.1, 0.9), "npc")),males_plot),ncol=3,widths=c(1,0.01,1)),
                       top=textGrob("FAMD Kmeans",
                                    gp=gpar(fontsize=20, col="black",fontface="bold")))


x11()
grid.draw(final_plot)


#####################################################################################################################
#####################################################################################################################
# GOWER DIANA 
#####################################################################################################################
#####################################################################################################################


################################################################################
# LOAD NECESSARY RESULTS
################################################################################



gower_diana_multi_morbid_female=
  readRDS("../real_results_from_HPC/results_used_for_final_figures/supplementary_figure_5/gower_diana_multi_morbid_female.rds")


gower_diana_multi_morbid_male=
  readRDS("../real_results_from_HPC/results_used_for_final_figures/supplementary_figure_5/gower_diana_multi_morbid_male.rds")


multi_morbid_female=readRDS("../data/processed_V3_females/multi_morbid_ordinal_factors_HW_mod_female_subset.rds")

multi_morbid_male=readRDS("../data/processed_V3_males/multi_morbid_ordinal_factors_HW_mod_male_subset.rds")


disease_variables=colnames(multi_morbid_female)[2:10]
disease_variables=disease_variables[-6]


data=multi_morbid_female[,disease_variables]
colnames(data)=c("CAD","Stroke","Morbidly_obese","Diabetes","Hypertension","Heart_failure","Intracranial_haemorrhage","Peripheral_vascular")
classes=cutree(gower_diana_multi_morbid_female,2)
custom_theme=theme_jh


plot_list=list()

for (k in 1:ncol(data)) {
  
  
  tmp_plot_data=as.data.frame(prop.table(table(data.frame(data[,k,drop=FALSE], classes)),margin =2))
  
  tmp_plot_data$n_ind=as.data.frame(table(data.frame(data[,k,drop=FALSE], classes)))[,3]
  tmp_plot_data=tmp_plot_data %>%
    arrange(.[[2]], desc(.[[1]]))
  tmp_plot_data$position=as.data.frame((tmp_plot_data %>% group_by(classes) %>% dplyr::mutate(cumsum(Freq))))[,5]
  tmp_plot_data$position=tmp_plot_data$position - tmp_plot_data$Freq/2
  
  
  if (k==1) {
    
    tmp_plot=ggplot(data=tmp_plot_data) +
      geom_bar(aes_string(x="classes",y="Freq",fill=paste0(colnames(data)[k])),stat="identity",position="stack") +
      xlab("") + ylab("") + custom_theme + geom_text(aes(x=classes,y=position,label=n_ind),check_overlap = TRUE,
                                                     size = 4,color="black") + ggtitle(colnames(data)[k]) +
      scale_fill_manual("Diagnosis of disease of interest", label=c("No","Yes"),values=gg_color_hue(2),
                        breaks=levels(tmp_plot_data$CAD))
    
    
    legend <- get_legend( tmp_plot)
    tmp_plot <-  tmp_plot + theme(legend.position="none")
    
  } else {
    
    tmp_plot=ggplot(data=tmp_plot_data) +
      geom_bar(aes_string(x="classes",y="Freq",fill=paste0(colnames(data)[k])),stat="identity",position="stack") +
      xlab("") + ylab("") + custom_theme + geom_text(aes(x=classes,y=position,label=n_ind),check_overlap = TRUE,
                                                     size = 4,color="black") + ggtitle(colnames(data)[k]) +
      theme(legend.position="none")
    
  }
  
  plot_list[[k]]=tmp_plot
}


plot_list=c(plot_list,list(legend))

females_plot=gTree(children = gList(rectGrob(gp=gpar(fill="white",col="white", lwd=0)),
                                    arrangeGrob(arrangeGrob(grobs=c(plot_list), ncol=3, nrow=3),
                                                top = textGrob(paste0("Women"),
                                                               gp=gpar(font=2,fontsize=15)))))




data=multi_morbid_male[,disease_variables]
colnames(data)=c("CAD","Stroke","Morbidly_obese","Diabetes","Hypertension","Heart_failure","Intracranial_haemorrhage","Peripheral_vascular")
classes=cutree(gower_diana_multi_morbid_male,2)
custom_theme=theme_jh


plot_list=list()

for (k in 1:ncol(data)) {
  
  
  tmp_plot_data=as.data.frame(prop.table(table(data.frame(data[,k,drop=FALSE], classes)),margin =2))
  
  tmp_plot_data$n_ind=as.data.frame(table(data.frame(data[,k,drop=FALSE], classes)))[,3]
  tmp_plot_data=tmp_plot_data %>%
    arrange(.[[2]], desc(.[[1]]))
  tmp_plot_data$position=as.data.frame((tmp_plot_data %>% group_by(classes) %>% dplyr::mutate(cumsum(Freq))))[,5]
  tmp_plot_data$position=tmp_plot_data$position - tmp_plot_data$Freq/2
  
  
  if (k==1) {
    
    tmp_plot=ggplot(data=tmp_plot_data) +
      geom_bar(aes_string(x="classes",y="Freq",fill=paste0(colnames(data)[k])),stat="identity",position="stack") +
      xlab("") + ylab("") + custom_theme + geom_text(aes(x=classes,y=position,label=n_ind),check_overlap = TRUE,
                                                     size = 4,color="black") + ggtitle(colnames(data)[k]) +
      scale_fill_manual("Diagnosis of disease of interest", label=c("No","Yes"),values=gg_color_hue(2),
                        breaks=levels(tmp_plot_data$CAD))
    
    
    legend <- get_legend( tmp_plot)
    tmp_plot <-  tmp_plot + theme(legend.position="none")
    
  } else {
    
    tmp_plot=ggplot(data=tmp_plot_data) +
      geom_bar(aes_string(x="classes",y="Freq",fill=paste0(colnames(data)[k])),stat="identity",position="stack") +
      xlab("") + ylab("") + custom_theme + geom_text(aes(x=classes,y=position,label=n_ind),check_overlap = TRUE,
                                                     size = 4,color="black") + ggtitle(colnames(data)[k]) +
      theme(legend.position="none")
    
  }
  
  plot_list[[k]]=tmp_plot
}


plot_list=c(plot_list,list(legend))


males_plot=gTree(children = gList(rectGrob(gp=gpar(fill="white",col="white", lwd=0)),
                                  arrangeGrob(arrangeGrob(grobs=c(plot_list), ncol=3, nrow=3),
                                              top = textGrob(paste0("Men"),
                                                             gp=gpar(font=2,fontsize=16)))))





final_plot=arrangeGrob(rectGrob(gp = gpar(col = "white")),heights=c(0.025,1),
                       arrangeGrob(grobs=list(females_plot,linesGrob(x = unit(c(0.5, 0.5), "npc"),
                                                                   y = unit(c(0.1, 0.9), "npc")),males_plot),ncol=3,widths=c(1,0.01,1)),
                       top=textGrob("Gower Diana",
                                    gp=gpar(fontsize=20, col="black",fontface="bold")))


x11()
grid.draw(final_plot)
