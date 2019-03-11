
########################################################################################################
# Retrieve the parangon indivudal from a cluster (works only with continuous variables and euclidian distance)
#### INPUTS
# data : all data points within the given cluster
#### OUTPUT
# indice : index of the parangon individual
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
# Plots a silhouette plot (cluster integrity)
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


########################################################################################################
# Plot the means for each continuous variable of the dataset by cluster
#### INPUTS
# data : continuous variables
# classes : clusters
# color_scale : color scale for the plot
# title : title of the plot
# custom_theme : custom ggplot theme to be applied
#### OUTPUT
# ggplot object of the means by cluster plot
########################################################################################################


data=multi_morbid[,cont_variables]
classes=as.factor(clusters_kmeans_FAMD_multi_morbid)
color_scale=NULL
custom_theme=theme_jh
title=NULL


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


########################################################################################################
# Plot the distibution for each categorical variable of the dataset by cluster
#### INPUTS
# data : categorical variables (= true factors)
# classes : clusters
# layout : layout for the plots
# color_scale : color scale for the plot
# title : title of the plot
# custom_theme : custom ggplot theme to be applied
#### OUTPUT
# grid object containing all that's needed for the plots

########################################################################################################


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
      xlab("") + ylab("") + custom_theme + geom_shadowtext(aes(x=classes,y=position,label=n_ind),check_overlap = TRUE,
                      size = 4)
    
    
    
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


########################################################################################################
# Plot the distibution for each categorical variable of the dataset by cluster
#### INPUTS
# data : categorical variables (= true factors)
# classes : clusters
# layout : layout for the plots
# color_scale : color scale for the plot
# title : title of the plot
# custom_theme : custom ggplot theme to be applied
#### OUTPUT
# grid object containing all that's needed for the plots
########################################################################################################

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



########################################################################################################
# Criterion plot : plot the given criterion over the sequence of numbers of clusters
#### INPUTS
# data : 
# classes : clusters
# color_scale : color scale for the plot
# title : title of the plot
# custom_theme : custom ggplot theme to be applied
#### OUTPUT
# ggplot object of the means by cluster plot
########################################################################################################
