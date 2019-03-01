require(limma)
require(VennDiagram)
require(UpSetR)
setwd('~/Documents/Translational/Barracudas/')
mydata = read.csv('../data/processed/UKBcompleteFeb19.csv')

#define morbidly obese
mydata$obese = ifelse(mydata$BMI > 35, 1, 0)

#define outcome col names andn dataset
outcomes = c('diabetes','mi','stroke','htn','angina','obese')

outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(mydata))

#upset plot whole dataset
png('plots/disease_intersections.png',width=1500,height=800)
upset(mydata, 
      sets = outcomes,
      sets.bar.color = "#56B4E9",
      order.by = c("freq"), 
      empty.intersections = "on",
      mainbar.y.label = 'Disease Intersections',
      text.scale = c(2, 1.5, 1.5, 1.5, 2, 1.5))
dev.off()

#col of chronic diseases
no_chronic = apply(mydata[,outcome_cols],1,sum)

#predictors
multi_morbid = mydata[which(no_chronic>1),]

#upset plot just multi morbid
svg('plots/multi_morbid_disease_intersections.svg',width=20,height=10)
upset(multi_morbid, 
      sets = outcomes,
      sets.bar.color = "#56B4E9",
      order.by = c("freq"), 
      empty.intersections = "on",
      mainbar.y.label = 'Disease Intersections',
      text.scale = c(2, 1.5, 1.5, 1.5, 2, 1.5))
dev.off()

