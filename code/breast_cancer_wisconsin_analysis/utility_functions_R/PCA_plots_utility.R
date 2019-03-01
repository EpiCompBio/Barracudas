######################################################################
# TESTING FUNCTION OF THIS SCRIPT
######################################################################

# distinct_scale=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A6CEE3")
# # distinct_scale=c("#00CCFF","#FF00FF","#66FF00","#FF9900","#FF0000","#3399FF","#990066","#006600","#663300","#FFCC33")
# 
# 
# theme_jh = theme_grey() + theme(plot.title=element_text(family="serif",face="bold",color="#8d0e00",hjust=0.5,vjust=0.5,size=16)) +
#   theme(panel.background = element_rect(fill='#f9f9f9', color='white')) +
#   theme(axis.title = element_text(family="serif",face="bold",size=12,hjust=0.5)) +
#   theme(axis.line = element_line(color = "#747474",arrow=arrow(angle = 30,length = unit(0.10, "inches")))) +
#   theme(legend.background = element_rect(fill='#f9f9f9', color='black')) +
#   theme(legend.text = element_text(family="serif",hjust=0.5)) +
#   theme(legend.title = element_text(family="serif",face="bold",hjust=0.5)) +
#   theme(legend.key.size = unit(1,"line")) +
#   theme(plot.margin=unit(c(.5, .5, .5, .5), "cm"))
# 
# 
# iris=data.frame(iris)
# iris_PCA=PCA(iris[,1:4],graph=FALSE)
# 
# #Correlation circle
# iris_PCA_correlation_circle=make_PCA_correlation_circle(iris_PCA,dims=c(1,2),custom_theme=theme_jh)
# x11()
# print(iris_PCA_correlation_circle)
# 
# #Ind plot
# iris_PCA_ind_plot=make_PCA_ind_plot(iris_PCA,dims=c(1,3),custom_theme=theme_jh)
# x11()
# print(iris_PCA_ind_plot)
# 
# #Biplot
# iris_PCA_biplot=make_PCA_biplot(iris[,1:4],dims=c(1,2),iris_PCA,color_scale=distinct_scale,theme_jh)
# x11()
# print(iris_PCA_biplot)
# 
# #Ind plot with classes
# iris_PCA_ind_classes=make_PCA_ind_plot_classes(iris_PCA,dims=c(1,2),classes=iris$Species,custom_theme=NULL,color_scale=distinct_scale)
# x11()
# print(iris_PCA_ind_classes)
# 
# #Biplot
# iris_PCA_biplot_classes=make_PCA_biplot_classes(iris[,1:4],iris_PCA,dims=c(1,2),classes=iris$Species,color_scale=NULL,theme_jh)
# x11()
# print(iris_PCA_biplot_classes)


using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using("FactoMineR","ggplot2")


circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}


########################################################################################################
#Correlation plot of PCA results in the space of the two first components

####
#INPUTS#
#PCA : result of the PCA function from FactoMineR
#dims : componets to select for the representation
#color_scale : color scale to be used
#custom_theme : custom theme to apply to the ggplot
####
#OUTPUT#
#Correlation plot for the variables for the first two components : ggplot object
########################################################################################################
make_PCA_correlation_circle<- function(PCA,dims=c(1,2),custom_theme=NULL) {
  
  PCA_correlation_data=as.data.frame(PCA$var$coord[,dims])
  rect_data=as.data.frame(cbind(c(-1.2,-1.2,1.2,1.2,-1.2),c(-1.2,1.2,1.2,-1.2,-1.2)))
  colnames(rect_data)=c("x","y")
  dat <- circleFun(c(0,0),2,npoints = 1000)
  
  PCA_plot <- ggplot(data=PCA_correlation_data,aes_string(x=paste0("Dim.",dims[1]), y=paste0("Dim.",dims[2]))) +
    geom_path(data=dat,aes(x=x,y=y),color="#A6CEE3",size=1) +
    geom_path(data=rect_data,aes(x=x,y=y),color="#A6CEE3",size=0.5) +
    geom_hline(aes(0), size=.2,yintercept=0) + geom_vline(aes(0), size=.2,xintercept=0) +
    coord_equal() + geom_text(label=rownames(PCA_correlation_data),size = 3,color="#1F78B4") +
    geom_segment(aes_string(x=0, y=0, xend=paste0("Dim.",dims[1]), yend=paste0("Dim.",dims[2])),arrow=arrow(length=unit(0.2,"cm")),color="#1F78B4",size=0.5) + 
    xlab(paste0("Dim. ",dims[1], " axis : ", round(PCA$eig[dims[1],2],2)," % variance explained",sep="")) +
    ylab (paste0("Dim. ",dims[2], " axis : ",round(PCA$eig[dims[2],2],2)," % variance explained",sep="")) +
    labs(title="PCA Correlation Plot") + custom_theme
  return(PCA_plot)
}


########################################################################################################
#PCA Individuals plot in the space of the two first components

####
#INPUTS#
#PCA : result of the PCA function from FactoMineR
#dims : componets to select for the representation
#color_scale : color scale to be used
#custom_theme : custom theme to apply to the ggplot
####
#OUTPUT#
#PCA Individuals plot in the space of the two first components : ggplot object
########################################################################################################

# PCA=iris_PCA
# dims=c(1,3)

make_PCA_ind_plot<- function(PCA,dims=c(1,2),custom_theme=NULL) {
  
  
  PCA_ind_plot_data=data.frame(PCA$ind$coord[,dims],names=rownames(PCA$ind$coord))
  PCA_ind_plot=ggplot(data=PCA_ind_plot_data) +
    geom_point(aes_string(x=paste0("Dim.",dims[1]),y=paste0("Dim.",dims[2])),color="#0a9cd4") +
    geom_text(aes_string(x=paste0("Dim.",dims[1]),y=paste0("Dim.",dims[2]),label="names"),color="#0a9cd4") +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=0) +
    xlab(paste0("Dim. ",dims[1], " axis : ", round(PCA$eig[dims[1],2],2)," % variance explained",sep="")) +
    ylab (paste0("Dim. ",dims[2], " axis : ",round(PCA$eig[dims[2],2],2)," % variance explained",sep="")) +
    ggtitle("PCA Individuals plot") + custom_theme
  
  return(PCA_ind_plot)
}

########################################################################################################
#Biplot of PCA results in the space of the two first components (Individuals, Variables, Outcome)

####
#INPUTS#
#predictors : predictors
#pca_res : result of the PCA function from FactoMineR
#dims : componets to select for the representation
#color_scale : color scale to be used
#custom_theme : custom theme to apply to the ggplot
####
#OUTPUT#
#Biplot of PLS results in the space of the two first components (Individuals, Variables, Outcome)
########################################################################################################

make_PCA_biplot=function(predictors,pca_res,dims=c(1,2),color_scale=NULL,custom_theme=NULL)  {
  
  
  #Draw a circle on ggplot
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  #Simulate ggplot2 standard colors
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  
  
  biplot_data_tscores=data.frame(pca_res$ind$coord[,dims],rownames(pca_res$ind$coord),"Individual")
  colnames(biplot_data_tscores)[3:4]=c("Name","Type")
  biplot_data_Xvar_cor=data.frame(cor(predictors,biplot_data_tscores[,1:2])*max(biplot_data_tscores[,1:2])/2,colnames(predictors),"Variable")
  colnames(biplot_data_Xvar_cor)[3:4]=c("Name","Type")
  
  biplot_data=rbind(biplot_data_tscores,biplot_data_Xvar_cor)
  
  data_circle <- circleFun(c(0,0),max(biplot_data_tscores[,1:2]),npoints = 100)
  
  
  full_results_biplot_PCA=ggplot(data=biplot_data)
  full_results_biplot_PCA=full_results_biplot_PCA + geom_point(aes_string(x=paste0("Dim.",dims[1]),y=paste0("Dim.",dims[2]),color="Type")) +
    geom_segment(data=subset(biplot_data,Type=="Variable"),
                 aes_string(x=0, y=0, xend=paste0("Dim.",dims[1]), yend=paste0("Dim.",dims[2]),color="Type"),arrow=arrow(length=unit(0.2,"cm"))) +
    geom_text(data=biplot_data,aes_string(x=paste0("Dim.",dims[1]),y=paste0("Dim.",dims[2]),label="Name",color="Type")) +
    geom_hline(yintercept=0) + geom_vline(xintercept=0) + 
    xlab(paste0("Dim. ",dims[1], " axis : ", round(PCA$eig[dims[1],2],2)," % variance explained",sep="")) +
    ylab (paste0("Dim. ",dims[2], " axis : ",round(PCA$eig[dims[2],2],2)," % variance explained",sep="")) 
  
  if (!is.null(color_scale)) {
    full_results_biplot_PCA=full_results_biplot_PCA + scale_color_manual("Type",values=color_scale[1:3])
    full_results_biplot_PCA=full_results_biplot_PCA + geom_path(data=data_circle,aes(x=x,y=y),color=color_scale[2])
  } else {
    full_results_biplot_PCA=full_results_biplot_PCA + geom_path(data=data_circle,aes(x=x,y=y),color=gg_color_hue(2)[2])
  }
  full_results_biplot_PCA=full_results_biplot_PCA + ggtitle("PCA Biplot") + custom_theme
  
  return(full_results_biplot_PCA)
  
}


########################################################################################################
#PCA Individuals plot in the space of the two first components

####
#INPUTS#
#PCA : result of the PCA function from FactoMineR
#dims : componets to select for the representation
#classes : classes for the colors of the points
#color_scale : color scale to be used
#custom_theme : custom theme to apply to the ggplot
####
#OUTPUT#
#PCA Individuals plot in the space of the two first components : ggplot object
########################################################################################################



make_PCA_ind_plot_classes <- function(PCA,dims=c(1,2),classes,custom_theme=NULL,color_scale=NULL,write_labels=TRUE) {
  
  
  PCA_ind_plot_classes_data=data.frame(PCA$ind$coord[,dims],names=rownames(PCA$ind$coord),classes=classes)
  PCA_ind_plot_classes=ggplot(data=PCA_ind_plot_classes_data) +
    geom_point(aes_string(x=paste0("Dim.",dims[1]),y=paste0("Dim.",dims[2]),color="classes")) +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=0) +
    xlab(paste0("Dim. ",dims[1], " axis : ", round(PCA$eig[dims[1],2],2)," % variance explained",sep="")) +
    ylab (paste0("Dim. ",dims[2], " axis : ",round(PCA$eig[dims[2],2],2)," % variance explained",sep="")) +
    ggtitle("PCA Individuals plot") + custom_theme
  
  if (!is.null(color_scale)) {
    PCA_ind_plot_classes=PCA_ind_plot_classes + scale_color_manual("Classes",values=color_scale[1:length(levels(classes))])
  }
  if (write_labels==TRUE) {
    PCA_ind_plot_classes= PCA_ind_plot_classes + 
      geom_text(aes_string(x=paste0("Dim.",dims[1]),y=paste0("Dim.",dims[2]),label="names",color="classes"))
  }
  
  return(PCA_ind_plot_classes)
}



########################################################################################################
#Biplot of PCA results in the space of the two first components (Individuals, Variables, Outcome)

####
#INPUTS#
#predictors : predictors
#pca_res : result of the PCA function from FactoMineR
#dims : componets to select for the representation
#classes : classes to color
#color_scale : color scale to be used
#custom_theme : custom theme to apply to the ggplot
####
#OUTPUT#
#Biplot of PLS results in the space of the two first components (Individuals, Variables, Outcome)
########################################################################################################


# predictors=iris[,1:4]
# pca_res=iris_PCA
# dims=c(1,2)
# color_scale=distinct_scale
# color_scale=NULL
# custom_theme=theme_jh


make_PCA_biplot_classes=function(predictors,pca_res,dims=c(1,2),classes,color_scale=NULL,custom_theme=NULL)  {
  
  
  #Draw a circle on ggplot
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  #Simulate ggplot2 standard colors
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  
  
  biplot_data_tscores=data.frame(pca_res$ind$coord[,dims],rownames(pca_res$ind$coord),classes)
  colnames(biplot_data_tscores)[3:4]=c("Name","Type")
  biplot_data_Xvar_cor=data.frame(cor(predictors,biplot_data_tscores[,1:2])*max(biplot_data_tscores[,1:2])/2,colnames(predictors),"Variable")
  colnames(biplot_data_Xvar_cor)[3:4]=c("Name","Type")
  
  biplot_data=rbind(biplot_data_tscores,biplot_data_Xvar_cor)
  
  data_circle <- circleFun(c(0,0),max(biplot_data_tscores[,1:2]),npoints = 100)
  
  
  full_results_biplot_PCA_classes=ggplot(data=biplot_data)
  full_results_biplot_PCA_classes=full_results_biplot_PCA_classes + geom_point(aes_string(x=paste0("Dim.",dims[1]),y=paste0("Dim.",dims[2]),color="Type")) +
    geom_segment(data=subset(biplot_data,Type=="Variable"),
                 aes_string(x=0, y=0, xend=paste0("Dim.",dims[1]), yend=paste0("Dim.",dims[2]),color="Type"),arrow=arrow(length=unit(0.2,"cm"))) +
    geom_text(data=biplot_data,aes_string(x=paste0("Dim.",dims[1]),y=paste0("Dim.",dims[2]),label="Name",color="Type")) +
    geom_hline(yintercept=0) + geom_vline(xintercept=0) + 
    xlab(paste0("Dim. ",dims[1], " axis : ", round(PCA$eig[dims[1],2],2)," % variance explained",sep="")) +
    ylab (paste0("Dim. ",dims[2], " axis : ",round(PCA$eig[dims[2],2],2)," % variance explained",sep="")) 
  
  if (!is.null(color_scale)) {
    full_results_biplot_PCA_classes=full_results_biplot_PCA_classes + scale_color_manual("Type",values=color_scale[1:(length(levels(classes))+1)])
    full_results_biplot_PCA_classes=full_results_biplot_PCA_classes + geom_path(data=data_circle,aes(x=x,y=y),color=color_scale[4])
  } else {
    full_results_biplot_PCA_classes=full_results_biplot_PCA_classes +
      geom_path(data=data_circle,aes(x=x,y=y),color=gg_color_hue((length(levels(classes))+1))[(length(levels(classes))+1)])
  }
  full_results_biplot_PCA_classes=full_results_biplot_PCA_classes + ggtitle("PCA Biplot") + custom_theme
  
  return(full_results_biplot_PCA_classes)
  
}





