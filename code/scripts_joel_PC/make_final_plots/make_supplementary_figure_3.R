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

using("ggplot2","grid","gridExtra","ggrepel","dendextend")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
################################################################################
# LOAD NECESSARY RESULTS
################################################################################


gower_diana_multi_morbid_female=
  readRDS("../real_results_from_HPC/results_used_for_final_figures/supplementary_figure_3/gower_diana_multi_morbid_female.rds")

gower_diana_multi_morbid_male=
  readRDS("../real_results_from_HPC/results_used_for_final_figures/supplementary_figure_3/gower_diana_multi_morbid_male.rds")


#Transform into dendrograms
dendrogram_gower_diana_multi_morbid_female_V3=as.dendrogram(gower_diana_multi_morbid_female)
dendrogram_gower_diana_multi_morbid_male_V3=as.dendrogram(gower_diana_multi_morbid_male)

#Color by cluster membership
dendrogram_gower_diana_multi_morbid_female_V3=dendrogram_gower_diana_multi_morbid_female_V3 %>% color_branches(k = 2,col=gg_color_hue(2))
dendrogram_gower_diana_multi_morbid_male_V3=dendrogram_gower_diana_multi_morbid_male_V3 %>% color_branches(k = 2,col=rev(gg_color_hue(2)))


x11()
plot(dendrogram_gower_diana_multi_morbid_female_V3,leaflab = "none",type = "rectangle", ylab = "Height")

x11()
plot(rev(dendrogram_gower_diana_multi_morbid_male_V3),leaflab = "none",type = "rectangle", ylab = "Height")



