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

using("ggplot2","dplyr","tidyr","scales","shadowtext")

################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################


file_path<-dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(file_path)

source("C:/Users/JOE/Documents/R_utility_and_self_implementations/colors_themes_utility.R")

################################################################################
# PRE-PROCESSING
################################################################################

mydata = read.csv('../../../data/processed/UKBcompleteFeb19.csv')

#define obese BMI > 35
mydata$obese = ifelse(mydata$BMI >= 35, 1, 0)

#define outcome cols
outcomes = c('diabetes','mi','stroke','angina','obese')

outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(mydata))

#col of chronic diseases
mydata$no_chronic = apply(mydata[,outcome_cols],1,sum)

#change gender levels
mydata$Sex = factor(ifelse(mydata$gender == 0, 'Female','Male'))

#binary cols
binary_cols = which(unlist(sapply(mydata, function(x) length(levels(factor(x)))==2)))

#change binary col class
mydata[,binary_cols] = sapply(mydata[,binary_cols], factor)

#subset multi morbid rows
multi_morbid = mydata[which(mydata$no_chronic>1),]

################################################################################
#Proportion of chronic diseases by alcohol consumption and sex barplot
################################################################################

prop_dis_alc_sex_plot_data=mydata[,c("current_etoh","Sex","no_chronic")] %>%
  group_by(no_chronic,current_etoh,Sex) %>% summarise(n_ind=length(Sex)) %>% as.data.frame()

prop_dis_alc_sex_plot_data$proportion=apply(prop_dis_alc_sex_plot_data,1,function(x) {
  return(as.numeric(x[4])/ sum((prop_dis_alc_sex_plot_data[((prop_dis_alc_sex_plot_data[,1]==as.numeric(x[1])) &
                                                  (prop_dis_alc_sex_plot_data[,2]==as.numeric(x[2]))),4])))
})

colnames(prop_dis_alc_sex_plot_data)[5]="proportion"

prop_dis_alc_sex_plot_data$Sex_position=ifelse(prop_dis_alc_sex_plot_data$Sex=="Male",0.1,0.9)

prop_dis_alc_sex_plot=ggplot(prop_dis_alc_sex_plot_data) +
  geom_bar(aes(x = current_etoh,fill = Sex,y=proportion),stat="identity",width=0.7) + 
  geom_shadowtext(aes(x=current_etoh,y=Sex_position,label=n_ind),check_overlap = TRUE,
                  size = 4) + 
  facet_wrap(~ no_chronic) +
  scale_fill_manual(values=distinct_scale) +
  labs(title = 'Number of chronic diseases',
       x = 'Current Alcohol Consumption',
       y = 'Male/Female proportion') + theme_jh



svg("../../descriptive_analysis_results/descritptive_analysis_joel_plots/prop_n_disease_by_alcohol_by_sex_plot.svg")
print(prop_dis_alc_sex_plot)
dev.off()


################################################################################
#Proportion of chronic diseases by smoking status and sex barplot
################################################################################



prop_dis_smoking_sex_plot_data=mydata[,c("current_smoker","Sex","no_chronic")] %>%
  group_by(no_chronic,current_smoker,Sex) %>% summarise(n_ind=length(Sex)) %>% as.data.frame()

prop_dis_smoking_sex_plot_data$proportion=apply(prop_dis_smoking_sex_plot_data,1,function(x) {
  return(as.numeric(x[4])/ sum((prop_dis_smoking_sex_plot_data[((prop_dis_smoking_sex_plot_data[,1]==as.numeric(x[1])) &
                                                              (prop_dis_smoking_sex_plot_data[,2]==as.numeric(x[2]))),4])))
})



colnames(prop_dis_smoking_sex_plot_data)[5]="proportion"

prop_dis_smoking_sex_plot_data$Sex_position=ifelse(prop_dis_smoking_sex_plot_data$Sex=="Male",0.1,0.9)

prop_dis_smoking_sex_plot=ggplot(prop_dis_smoking_sex_plot_data) +
  geom_bar(aes(x = current_smoker,fill = Sex,y=proportion),stat="identity",width=0.7) + 
  geom_shadowtext(aes(x=current_smoker,y=Sex_position,label=n_ind),check_overlap = TRUE,
                  size = 4) + 
  facet_wrap(~ no_chronic) +
  scale_fill_manual(values=distinct_scale) +
  labs(title = 'Number of chronic diseases',
       x = 'Current smokingohol Consumption',
       y = 'Male/Female proportion') + theme_jh




svg("../../descriptive_analysis_results/descritptive_analysis_joel_plots/prop_n_disease_by_smoking_by_sex_plot.svg")
print(prop_dis_smoking_sex_plot)
dev.off()