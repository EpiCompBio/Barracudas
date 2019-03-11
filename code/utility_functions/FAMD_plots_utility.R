########################################################################################################
# Loading 
########################################################################################################

using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using("FactoMineR","ggplot2","ggrepel")



circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}


########################################################################################################
# FAMD Individuals plot in the space of the chosen components

####
#INPUTS#
#FAMD : result of the FAMD function from FactoMineR
#dims : componets to select for the representation
#color_scale : color scale to be used
#custom_theme : custom theme to apply to the ggplot
####
#OUTPUT#
#FAMD Individuals plot in the space of the two first components : ggplot object
########################################################################################################

make_FAMD_ind_plot<- function(FAMD,dims=c(1,2),custom_theme=NULL,color_scale=NULL) {
  
  
  FAMD_ind_plot_data=data.frame(FAMD$ind$coord[,dims],names=rownames(FAMD$ind$coord))
  
  if (is.null(color_scale)) {
    FAMD_ind_plot=ggplot(data=FAMD_ind_plot_data) +
      geom_point(aes_string(x=paste0("Dim.",dims[1]),y=paste0("Dim.",dims[2])),color="#0a9cd4") +
      geom_text(aes_string(x=paste0("Dim.",dims[1]),y=paste0("Dim.",dims[2]),label="names"),color="#0a9cd4")
  } else {
    FAMD_ind_plot=ggplot(data=FAMD_ind_plot_data) +
      geom_point(aes_string(x=paste0("Dim.",dims[1]),y=paste0("Dim.",dims[2])),color=color_scale[1]) +
      geom_text(aes_string(x=paste0("Dim.",dims[1]),y=paste0("Dim.",dims[2]),label="names"),color=color_scale[1])
  }
  
  FAMD_ind_plot=FAMD_ind_plot + geom_hline(yintercept=0) +
    geom_vline(xintercept=0) +
    xlab(paste0("Dim. ",dims[1], " axis : ", round(FAMD$eig[dims[1],2],2)," % variance explained",sep="")) +
    ylab (paste0("Dim. ",dims[2], " axis : ",round(FAMD$eig[dims[2],2],2)," % variance explained",sep="")) +
    ggtitle("FAMD Individuals plot") + custom_theme + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
  
  return(FAMD_ind_plot)
}


########################################################################################################
# FAMD graph of the variables in the space of the chosen components

####
#INPUTS#
#FAMD : result of the FAMD function from FactoMineR
#dims : componets to select for the representation
#color_scale : color scale to be used
#custom_theme : custom theme to apply to the ggplot
####
#OUTPUT#
#FAMD graph of the variables in the space of the two first components : ggplot object
########################################################################################################

# FAMD=FAMD_res_example_mixed_data_clustering_1
# dims=c(1,2)
# custom_theme=theme_jh
# color_scale=NULL

make_FAMD_variable_graph<- function(FAMD,dims=c(1,2),custom_theme=NULL,color_scale=NULL) {
  
  FAMD_variable_graph_data=as.data.frame(FAMD$var$coord[,dims])
  
  if (is.null(color_scale)) {
    FAMD_variable_graph <- ggplot(data=FAMD_variable_graph_data,aes_string(x=paste0("Dim.",dims[1]), y=paste0("Dim.",dims[2]))) +
      geom_text_repel(label=rownames(FAMD_variable_graph_data),size = 3,color="#1F78B4") +
      geom_point(size = 3,color="#1F78B4")
  } else {
    FAMD_variable_graph <- ggplot(data=FAMD_variable_graph_data,aes_string(x=paste0("Dim.",dims[1]), y=paste0("Dim.",dims[2]))) +
      geom_text_repel(label=rownames(FAMD_variable_graph_data),size = 3,color=color_scale[1]) +
      geom_point(size = 3,color=color_scale[1])
  }
  FAMD_variable_graph=FAMD_variable_graph + 
    xlab(paste0("Dim. ",dims[1], " axis : ", round(FAMD$eig[dims[1],2],2)," % variance explained",sep="")) +
    ylab (paste0("Dim. ",dims[2], " axis : ",round(FAMD$eig[dims[2],2],2)," % variance explained",sep="")) +
    labs(title="FAMD Variable graph") + custom_theme + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
   
  return(FAMD_variable_graph)
 
}


########################################################################################################
# FAMD graph of individual levels of the categorical variables in the space of the chosen components

####
#INPUTS#
#FAMD : result of the FAMD function from FactoMineR
#dims : componets to select for the representation
#color_scale : color scale to be used
#custom_theme : custom theme to apply to the ggplot
####
#OUTPUT#
#FAMD graph of individual levels of the categorical variables in the space of the two first components : ggplot object
########################################################################################################

make_FAMD_cat_variable_graph<- function(FAMD,dims=c(1,2),custom_theme=NULL,color_scale=NULL) {
  
  FAMD_cat_variable_graph_data=as.data.frame(FAMD_res_example_mixed_data_clustering_1$quali.var$coord[,dims])
  
  if (is.null(color_scale)) {
    FAMD_cat_variable_graph <- ggplot(data=FAMD_cat_variable_graph_data,aes_string(x=paste0("Dim.",dims[1]), y=paste0("Dim.",dims[2]))) +
      geom_text_repel(label=rownames(FAMD_cat_variable_graph_data),size = 3,color="#1F78B4") +
      geom_point(size = 3,color="#1F78B4")
  } else {
    FAMD_cat_variable_graph <- ggplot(data=FAMD_cat_variable_graph_data,aes_string(x=paste0("Dim.",dims[1]), y=paste0("Dim.",dims[2]))) +
      geom_text_repel(label=rownames(FAMD_cat_variable_graph_data),size = 3,color=color_scale[1]) +
      geom_point(size = 3,color=color_scale[1])
  }
  FAMD_cat_variable_graph=FAMD_cat_variable_graph + 
    xlab(paste0("Dim. ",dims[1], " axis : ", round(FAMD$eig[dims[1],2],2)," % variance explained",sep="")) +
    ylab (paste0("Dim. ",dims[2], " axis : ",round(FAMD$eig[dims[2],2],2)," % variance explained",sep="")) +
    labs(title="FAMD categorical variables graph") + custom_theme + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
  
  return(FAMD_cat_variable_graph)
  
}


########################################################################################################
# FAMD Individuals plot in the space of the chosen components

####
#INPUTS#
#FAMD : result of the FAMD function from FactoMineR
#dims : componets to select for the representation
#color_scale : color scale to be used
#custom_theme : custom theme to apply to the ggplot
####
#OUTPUT#
#FAMD Individuals plot in the space of the two first components : ggplot object
########################################################################################################

# FAMD=FAMD_res_example_mixed_data_clustering_1
# classes=clusters_kmeans_FAMD
# dims=c(1,2)
# custom_theme=NULL
# color_scale=distinct_scale


make_FAMD_ind_plot_classes<- function(FAMD,classes,dims=c(1,2),custom_theme=NULL,color_scale=NULL) {
  
  FAMD_ind_plot_data=data.frame(FAMD$ind$coord[,dims],names=rownames(FAMD$ind$coord),clusters=as.factor(classes))
  
 FAMD_ind_plot=ggplot(data=FAMD_ind_plot_data) +
    geom_point(aes_string(x=paste0("Dim.",dims[1]),y=paste0("Dim.",dims[2]),color="clusters")) +
    geom_text(aes_string(x=paste0("Dim.",dims[1]),y=paste0("Dim.",dims[2]),color="clusters",label="names"),show.legend = FALSE) +
    guides(color = guide_legend(override.aes = list(size = 3)))
  
  if (!is.null(color_scale)) {
    FAMD_ind_plot= FAMD_ind_plot + scale_color_manual(values=color_scale)
  }
  
  FAMD_ind_plot=FAMD_ind_plot + geom_hline(yintercept=0) +
    geom_vline(xintercept=0) +
    xlab(paste0("Dim. ",dims[1], " axis : ", round(FAMD$eig[dims[1],2],2)," % variance explained",sep="")) +
    ylab (paste0("Dim. ",dims[2], " axis : ",round(FAMD$eig[dims[2],2],2)," % variance explained",sep="")) +
    ggtitle("FAMD Individuals plot") + custom_theme + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
  
  return(FAMD_ind_plot)
}


