########################################################################################################
# Make a circle

####
#INPUTS# 
#center : coordinates of the center of the circle
#diameter : diameter of the circle 
#npoints : number of points
####
#OUTPUT#
# a data frame allowing us to draw a circle
########################################################################################################

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
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
#Return the parangon individual for a given dataset (all the variables should be continuous)

####
#INPUTS#
# data : dataset
####
#OUTPUT#
# indice : rownumber of the parangon individual
########################################################################################################

parangon = function(data){
  moyenne = apply(data,2,mean)
  initialisation = sum((moyenne - data[1,])^2)
  indice = rownames(data)[1]
  for (i in 2:nrow(data)){
    if(sum((moyenne - data[i,])^2)<initialisation){
      initialisation = sum((moyenne - data[i,])^2)
      indice = rownames(data)[i]
    }
  }
  return(indice)
}



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