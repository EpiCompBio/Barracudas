########################################################################################################
########################################################################################################

make_density_plot=function(density_object,title=NULL,custom_theme=NULL) {
 
  using<-function(...) {
    libs<-unlist(list(...))
    req<-unlist(lapply(libs,require,character.only=TRUE))
    need<-libs[req==FALSE]
    if(length(need)>0){
      install.packages(need)
      lapply(need,require,character.only=TRUE)
    }
  }
  
  using("ggplot2")
  
 density_data=data.frame(x=density_object$x,y=density_object$y)
 density_plot=ggplot(density_data) + geom_line(aes(x=x,y=y),size=1,color="navy") +
   xlab("Value") + ylab("Density") + ggtitle(title) + custom_theme
 return(density_plot)
 # x11()
   # print(density_plot)
 
}

########################################################################################################
########################################################################################################

make_boxplot_factor=function(y,factor,title=NULL,custom_theme=NULL) {
  
data=data.frame(y=y,factor=factor)  
boxplot_by_factor=ggplot(data=data) + geom_boxplot(aes(y=y,color=factor)) + theme_jh
return(boxplot_by_factor)

}

########################################################################################################
########################################################################################################


GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

########################################################################################################
########################################################################################################

mean_by_cluster_continuous=function(data,classes,color_scale=NULL,custom_theme=NULL,title=NULL) {
  
  data_and_classes = data.frame(data, classes)
  means_clusters=aggregate(data_and_classes[,-c(ncol(data_and_classes))],
                           by=list(data_and_classes[,ncol(data_and_classes)]),mean)
  
  means_clusters=apply(means_clusters,2,as.numeric)
  
  plot_data=melt(setNames(data.frame(t(means_clusters)[-1,],id=1:(ncol(means_clusters)-1)),c(levels(classes),"id")),
                 id.var="id")
  
  
  mean_clusters_plot=ggplot(plot_data, aes(x=id,y=value,group=variable,colour=variable)) +
    geom_point() + geom_line(aes(lty=variable)) + 
    xlab("Variables") + ylab("Mean") +
    scale_colour_manual(name="Cluster", values = color_scale, labels = levels(classes)) +
    scale_linetype_discrete(name="Cluster",labels = color_scale) + theme(axis.text.x = element_text(angle = 90,size=10)) +
    custom_theme
  if (!is.null(title)) {
    mean_clusters_plot = mean_clusters_plot + ggtitle(title)
  }
  
  return(mean_clusters_plot)
}

########################################################################################################
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