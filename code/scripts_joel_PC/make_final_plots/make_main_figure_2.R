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

using("ggplot2","grid","gridExtra","ggrepel")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


source("C:/Users/JOE/Documents/R_utility_and_self_implementations/colors_themes_utility.R")
source("C:/Users/JOE/Documents/R_utility_and_self_implementations/clustering_utility.R")
source("../data/processed_V3_females/var_groupings_V3.R")

grouping_names=list(Disease=Disease,Demographics=Demographics,BMI_related=BMI_related,
                    Activity=Activity,Vital_signs=Vital_signs,Tobacco=Tobacco,
                    Alcohol=Alcohol,Dietary=Dietary,Med_surg_hx=Med_surg_hx)


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

theme_jh = theme_grey() + theme(plot.title=element_text(family="serif",face="bold",color="#8d0e00",hjust=0.5,vjust=0.5,size=16)) +
  theme(panel.background = element_rect(fill='#f9f9f9', colour='white')) +
  theme(axis.title = element_text(family="serif",face="bold",size=12,hjust=0.5)) +
  theme(axis.line = element_line(colour = "#747474",arrow=arrow(angle = 30,length = unit(0.10, "inches")))) +
  theme(legend.background = element_rect(fill='#f9f9f9', colour='black')) + 
  theme(legend.text = element_text(family="serif",hjust=0.5)) + 
  theme(legend.title = element_text(family="serif",face="bold",hjust=0.5)) +
  theme(legend.key.size = unit(1,"line")) +
  theme(plot.margin=unit(c(.5, .5, .5, .5), "cm"))

  
  
################################################################################
# LOAD NECESSARY RESULTS
################################################################################

##############################
#FEMALE
##############################

FAMD_kmeans_ordinal_factors_var_importance_stab_df_female=
  readRDS("../real_results_from_HPC/results_joel_HPC_V3_female/FAMD_kmeans_ordinal_factors/FAMD_kmeans_ordinal_continuous_var_importance_stab_df_morbid.rds")



gower_diana_multi_morbid_var_importance_stab_df_morbid_female=
  readRDS("../real_results_from_HPC/results_joel_HPC_V3_female/gower_diana/gower_diana_multi_morbid_var_importance_stab_df_morbid.rds")


kamila_var_importance_stab_df_morbid_female=
  readRDS("../real_results_from_HPC/results_joel_HPC_V3_female/kamila_ordinal_factors/kamila_var_importance_stab_df_morbid.rds")


variable_importance_stability_plot_1=make_variable_importance_stability_plot(FAMD_kmeans_ordinal_factors_var_importance_stab_df_female,
                                                                             grouping_names=grouping_names, color_scale=NULL,custom_theme=theme_jh,
                                                                             threshold=140,annotation_line=0) + ggtitle(NULL) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + scale_color_manual(labels = c("Disease","Demographics","BMI related","Activity","Vital signs",
                                                                      "Tobacco","Alcohol","Dietary","Med surg hx"),
                                                           breaks=c("Disease","Demographics","BMI_related","Activity","Vital_signs",
                                                                    "Tobacco","Alcohol","Dietary","Med_surg_hx"),values=gg_color_hue(9)) +
  xlab(NULL) + ylab(NULL) 




variable_importance_stability_plot_1=variable_importance_stability_plot_1 + theme(legend.position="none")

variable_importance_stability_plot_2=make_variable_importance_stability_plot(gower_diana_multi_morbid_var_importance_stab_df_morbid_female,
                                                                           grouping_names=grouping_names, color_scale=NULL,custom_theme=theme_jh,
                                                                           threshold=34,annotation_line=0) + theme(legend.position="none") + ggtitle(NULL) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + xlab(NULL) + ylab(NULL)

variable_importance_stability_plot_3=make_variable_importance_stability_plot(kamila_var_importance_stab_df_morbid_female,
                                                                           grouping_names=grouping_names, color_scale=NULL,custom_theme=theme_jh,
                                                                           threshold=340,annotation_line=0) + theme(legend.position="none") + ggtitle(NULL) +
  xlab(NULL) + ylab(NULL) + theme(axis.text.x=element_text(size=7,angle=60,hjust=1,vjust=1))

female_plots=list(variable_importance_stability_plot_1,variable_importance_stability_plot_2,variable_importance_stability_plot_3)




##############################
#MALE
##############################

FAMD_kmeans_ordinal_factors_var_importance_stab_df_male=
  readRDS("../real_results_from_HPC/results_joel_HPC_V3_male/FAMD_kmeans_ordinal_factors/FAMD_kmeans_ordinal_continuous_var_importance_stab_df_morbid.rds")



gower_diana_multi_morbid_var_importance_stab_df_morbid_male=
  readRDS("../real_results_from_HPC/results_joel_HPC_V3_male/gower_diana/gower_diana_multi_morbid_var_importance_stab_df_morbid.rds")


kamila_var_importance_stab_df_morbid_male=
  readRDS("../real_results_from_HPC/results_joel_HPC_V3_male/kamila_ordinal_factors/kamila_var_importance_stab_df_morbid.rds")


variable_importance_stability_plot_1=make_variable_importance_stability_plot(FAMD_kmeans_ordinal_factors_var_importance_stab_df_male,
                                                                             grouping_names=grouping_names, color_scale=NULL,custom_theme=theme_jh,
                                                                             threshold=320,annotation_line=0) + ggtitle(NULL) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + scale_color_manual(labels = c("Disease","Demographics","BMI related","Activity","Vital signs",
                                                                      "Tobacco","Alcohol","Dietary","Med surg hx"),
                                                           breaks=c("Disease","Demographics","BMI_related","Activity","Vital_signs",
                                                                    "Tobacco","Alcohol","Dietary","Med_surg_hx"),values=gg_color_hue(9)) +
  xlab(NULL) + ylab(NULL) + theme(legend.title = element_blank())


get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(variable_importance_stability_plot_1)


variable_importance_stability_plot_1=variable_importance_stability_plot_1 + theme(legend.position="none")

variable_importance_stability_plot_2=make_variable_importance_stability_plot(gower_diana_multi_morbid_var_importance_stab_df_morbid_male,
                                                                             grouping_names=grouping_names, color_scale=NULL,custom_theme=theme_jh,
                                                                             threshold=60,annotation_line=0) + theme(legend.position="none") + ggtitle(NULL) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + xlab(NULL) + ylab(NULL)

variable_importance_stability_plot_3=make_variable_importance_stability_plot(kamila_var_importance_stab_df_morbid_male,
                                                                             grouping_names=grouping_names, color_scale=NULL,custom_theme=theme_jh,
                                                                             threshold=600,annotation_line=0) + theme(legend.position="none") + ggtitle(NULL) +
  xlab(NULL) + ylab(NULL) + theme(axis.text.x=element_text(size=7,angle=60,hjust=1,vjust=1))

male_plots=list(variable_importance_stability_plot_1,variable_importance_stability_plot_2,variable_importance_stability_plot_3)



test=grid.arrange(arrangeGrob(grobs=female_plots, ncol=1, nrow=3,heights=c(2,2,3)),arrangeGrob(grobs=male_plots, ncol=1, nrow=3,heights=c(2,2,3)),
                  arrangeGrob(legend, ncol=1, nrow=1), widths=c(2,2,1),bottom = "Variable names",left="Variable importance")
x11(20,15)
grid.draw(test)

