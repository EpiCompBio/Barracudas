################################################################################
# LOADING LIBRARIES
################################################################################

# using<-function(...) {
#  libs<-unlist(list(...))
#   req<-unlist(lapply(libs,require,character.only=TRUE))
#  need<-libs[req==FALSE]
#  if(length(need)>0){
#    install.packages(need)
#    lapply(need,require,character.only=TRUE)
#  }
# }
# 
# using("magrittr","cluster","dplyr")

library(cluster,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(magrittr,lib.loc ="/home/jheller/anaconda3/lib/R/library")
library(dplyr,lib.loc ="/home/jheller/anaconda3/lib/R/library")


################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

# file_path<-dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(file_path)

# setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


full_data=read.csv("../data/processed/UKBcompleteFeb19_subset.csv",row.names=1)
#  full_data=read.csv("../data/processed/UKBcompleteFeb19.csv")


################################################################################
# PRE-PROCESSING
################################################################################

#define obese BMI > 35
full_data$obese = ifelse(full_data$BMI >= 35, 1, 0)

#define outcome cols
outcomes = c('diabetes','mi','stroke','angina','obese')

outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(full_data))

#col of chronic diseases
full_data$no_chronic = apply(full_data[,outcome_cols],1,sum)

#change gender levels and remove gender that is not used anymore
full_data$Sex = factor(ifelse(full_data$gender == 0, 'Female','Male'))
full_data$gender=NULL

#re-organize columns
full_data=full_data %>% dplyr::select(eid,mi,angina,stroke,htn,obese,no_chronic, everything())

#subset multi morbid rows
multi_morbid = full_data[which(full_data$no_chronic>1),]

full_data[,'no_chronic']=as.factor(full_data[,'no_chronic'])
multi_morbid[,'no_chronic']=as.factor(multi_morbid[,'no_chronic'])

full_data$dvt_asthma_copd_atopy=NULL
multi_morbid$dvt_asthma_copd_atopy=NULL


#binary cols
binary_col_ids = which(unlist(sapply(full_data, function(x) length(levels(factor(x)))==2)))
full_data[,binary_col_ids]=lapply(full_data[,binary_col_ids],as.factor)
multi_morbid[,binary_col_ids]=lapply(multi_morbid[,binary_col_ids],as.factor)

#######################################################
# Nominal considered continuous
#######################################################

# multi_morbid_cont=multi_morbid
# 
# for (k in 1:ncol(multi_morbid_cont)) {
#   if (class(multi_morbid_cont[,k])!="factor" & k!=1) {
#     multi_morbid_cont[,k]=scale(multi_morbid_cont[,k])
#   }
# }


#symmetrical binary cols
symm_cols = binary_col_ids[names(binary_col_ids)!='Sex']

#categorical cols
cat_cols = c('birth_month')
ord_cols = c('self_reported_surgery',
             'freq_climb_stairs_4wks',
             'freq_walked_for_pleasure_4wks',
             'Duration_pleasure_walks',
             'smokers_in_house',
             'oily_fish_intake',
             'non_oily_fish_intake',
             'processed_meat',
             'poultry',
             'beef_intake',
             'lamb_intake',
             'pork_intake',
             'cheese_intake',
             'salt_added_food',
             'varition_in_diet',
             'Alc_intake_freq',
             'seated_box_height'
)

cat_col_ids = which(colnames(multi_morbid) %in% cat_cols)
ord_col_ids = which(colnames(multi_morbid) %in% ord_cols)

multi_morbid[,cat_col_ids] = factor(multi_morbid[,cat_col_ids])
multi_morbid[,ord_col_ids] = lapply(multi_morbid[,ord_col_ids], function(x) factor(as.integer(x), ordered = TRUE))

#scale numeric features
multi_morbid[,-c(1,7,binary_col_ids,cat_col_ids,ord_col_ids)] =
  as.data.frame(scale(multi_morbid[,-c(1,7,binary_col_ids,cat_col_ids,ord_col_ids)]))

################################################################################
################################################################################
# multi-morbid individuals only
################################################################################
################################################################################



################################################################################
# FAMD  multi-morbid individuals
################################################################################

FAMD_multi_morbid_res=readRDS("../data/processed/FAMD_multi_morbid_res.rds")

nb_comp_FAMD_multi_morbid=which(FAMD_multi_morbid_res$eig[,3] > 90)[1]


################################################################################
# Gower distance for the proximty measures
################################################################################

gower_dissimilarity_multi_morbid_res=readRDS("../data/processed/gower_dissimilarity_multi_morbid_res.rds")



################################################################################
# Partitioning around medoids on the randomForest proximity measure
################################################################################

gower_pam_multi_morbid=pam(gower_dissimilarity_multi_morbid_res, 2)


saveRDS(gower_pam_multi_morbid,"../results/results_joel_HPC/gower_pam/gower_pam_multi_morbid.rds")


clusters_gower_pam_multi_morbid=gower_pam_multi_morbid$clustering

gower_pam_multi_morbid_plot_d12=make_FAMD_ind_plot_classes(FAMD_multi_morbid_res,classes=clusters_gower_pam_multi_morbid,
                                                        dims=c(1,2),
                                                        custom_theme=theme_jh,color_scale=distinct_scale)



gower_pam_multi_morbid_plot_d34=make_FAMD_ind_plot_classes(FAMD_multi_morbid_res,classes=clusters_gower_pam_multi_morbid,
                                                        dims=c(3,4),
                                                        custom_theme=theme_jh,color_scale=distinct_scale)


svg(filename="../results/results_joel_HPC/gower_pam/gower_pam_multi_morbid_plot_d12.svg",width=10,height=10)
print(gower_pam_multi_morbid_plot_d12)
dev.off()

svg(filename="../results/results_joel_HPC/gower_pam/gower_pam_multi_morbid_plot_d34.svg",width=10,height=10)
print(gower_pam_multi_morbid_plot_d34)
dev.off()

################################################
# Means continuous variables by cluster
################################################

cat_variables=colnames(multi_morbid_cont)[sapply(multi_morbid_cont,class) == "factor"]
cont_variables=colnames(multi_morbid_cont)[sapply(multi_morbid_cont,class) != "factor" & sapply(multi_morbid_cont,class) != "ordered factor"]
cont_variables=cont_variables[2:length(cont_variables)]


cat_variables=colnames(multi_morbid)[sapply(multi_morbid,class) == "factor" | sapply(multi_morbid,class) != "ordered factor"]
cont_variables=colnames(multi_morbid)[sapply(sapply(multi_morbid,class),function(x) {x[[1]]}) != "factor" &
                                         sapply(sapply(multi_morbid,class),function(x) {x[[1]]}) != "ordered"]
cont_variables=cont_variables[2:length(cont_variables)]

gower_pam_mean_by_cluster_continuous_plot=mean_by_cluster_continuous(data=multi_morbid[,cont_variables],
                                                                  classes=as.factor(clusters_gower_pam_multi_morbid),
                                                                  color_scale=NULL,custom_theme=theme_jh,title=NULL)


svg(filename="../results/results_joel_HPC/gower_pam/gower_pam_multi_morbid_mean_by_cluster_continuous_plot.svg",width=10,height=10)
print(gower_pam_mean_by_cluster_continuous_plot)
dev.off()


################################################
# Distributions Cat variables by cluster
################################################



cat_variables_split=splitIndices(nx=length(cat_variables), ncl=ceiling(length(cat_variables) / 9))

for (k in 1:length(cat_variables_split)) {
  
  
  gower_pam_cat_distribution_by_cluster=cat_distribution_by_cluster(data=multi_morbid[,cat_variables[cat_variables_split[[k]]]],
                                                                 classes=as.factor(clusters_gower_pam_multi_morbid),layout=c(3,3),
                                                                 color_scale=NULL,custom_theme=theme_jh,
                                                                 title=paste0("Distributions of categorical variables by classes (",
                                                                              k,"/",length(cat_variables_split),")"))
  
  
  svg(filename=paste0("../results/results_joel_HPC/gower_pam/gower_pam_multi_morbid_cat_distribution_by_cluster_",k,"_",length(cat_variables_split),".svg"),
      width=10,height=10)
  grid.draw(gower_pam_cat_distribution_by_cluster)
  dev.off()
  
}

################################################
# Distributions Cont variables by cluster
################################################

cont_variables_split=splitIndices(nx=length(cont_variables), ncl=ceiling(length(cont_variables) / 9))


for (k in 1:length(cont_variables_split)) {
  
  gower_pam_cont_distribution_by_cluster=cont_distribution_by_cluster(data=multi_morbid[,cont_variables[cont_variables_split[[k]]]],
                                                                   classes=as.factor(clusters_gower_pam_multi_morbid),layout=c(3,3),
                                                                   color_scale=NULL,custom_theme=theme_jh,
                                                                   title=paste0("Distributions of continuous variables by classes (",
                                                                                k,"/",length(cont_variables_split),")"))
  
  svg(filename=paste0("../results/results_joel_HPC/gower_pam/gower_pam_multi_morbid_cont_distribution_by_cluster_",k,"_",length(cont_variables_split),".svg"),
      width=10,height=10)
  grid.draw(gower_pam_cont_distribution_by_cluster)
  dev.off()
  
}