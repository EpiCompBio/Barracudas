using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using("viridis","RColorBrewer","ggplot2")



theme_jh = theme_grey() + theme(plot.title=element_text(family="serif",face="bold",color="#8d0e00",hjust=0.5,vjust=0.5,size=16)) +
  theme(panel.background = element_rect(fill='#f9f9f9', colour='white')) +
  theme(axis.title = element_text(family="serif",face="bold",size=12,hjust=0.5)) +
  theme(axis.line = element_line(colour = "#747474",arrow=arrow(angle = 30,length = unit(0.10, "inches")))) +
  theme(legend.background = element_rect(fill='#f9f9f9', colour='black')) + 
  theme(legend.text = element_text(family="serif",hjust=0.5)) + 
  theme(legend.title = element_text(family="serif",face="bold",hjust=0.5)) +
  theme(legend.key.size = unit(1,"line")) +
  theme(plot.margin=unit(c(.5, .5, .5, .5), "cm"))


#FULL color palettes 
##################################################################################################

#CONTINUOUS

#Purple continuous
purples_brewer_scale= list(color = brewer.pal(9, "Purples"))$color
#Spectral (rainbow) colors
spectral_brewer_scale= list(color = brewer.pal(9, "Spectral"))$color
#Blue continuous
blues_brewer_scale= list(color = brewer.pal(9, "Blues"))$color
#Viridis nice color scale
viridis_magma=magma(100)
#Viridis nice color scale
viridis_viridis=viridis(100)


#QUALITATIVE

#Nice colors for classification

# distinct_scale= list(color = brewer.pal(9, "Set1"))$color
# 
distinct_scale=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A6CEE3")
# 
# distinct_scale=c("#cc181e","#2793e8","#559900","#666666","#f1f1f1")
#
#distinct_scale= list(color = brewer.pal(8, "Dark2"))$color
#
# distinct_scale=c("#00CCFF","#FF00FF","#66FF00","#FF9900","#FF0000","#3399FF","#990066","#006600","#663300","#FFCC33")
#
#distinct_scale=c("navy","tomato", "forestgreen", "gold", "orange", "skyblue")
#
# distinct_scale=c( "gold", "orange", "skyblue","navy","tomato", "forestgreen")
#
# distinct_scale=list(color = brewer.pal(10, "Paired"))$color
##################################################################################################


#NICE SINGLE colors
##################################################################################################

nice_single_color="#0a9cd4"

