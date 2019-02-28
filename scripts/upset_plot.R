require(limma)
require(VennDiagram)
require(UpSetR)
setwd('~/Documents/Translational/Barracudas/')
mydata = read.csv('../data/processed/UKBcompleteFeb19.csv')

outcomes = c('diabetes','mi','stroke','dvt_asthma_copd_atopy','angina','htn')

outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(mydata))

outcome_dataset = mydata[,outcome_cols]

png('disease_intersections.png',width=1500,height=800)
upset(mydata, 
      sets = c('diabetes','mi','stroke','dvt_asthma_copd_atopy','angina','htn'),
      sets.bar.color = "#56B4E9",
      order.by = c("freq"), 
      empty.intersections = "on",
      mainbar.y.label = 'Disease Intersections',
      text.scale = c(2, 1.5, 1.5, 1.5, 2, 1.5))
dev.off()