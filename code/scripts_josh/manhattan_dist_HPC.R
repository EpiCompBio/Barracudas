lib.path = '/rds/general/user/je108/home/anaconda3/lib/R/library/'

suppressPackageStartupMessages(library(cluster, lib.loc = lib.path
))

setwd('/rds/general/project/medbio-berlanga-group/live/projects/group_multi_morbidity/')

mydata = read.csv('data/processed/UKBcompleteFeb19.csv',header=T)

#define obese BMI > 35
mydata$obese = ifelse(mydata$BMI >=35, 1, 0)

#define outcome col names andn dataset
outcomes = c('diabetes','mi','stroke','angina','obese')

outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(mydata))

#col of chronic diseases
no_chronic = apply(mydata[,outcome_cols],1,sum)

#predictors
multi_morbid = mydata[which(no_chronic>1),-c(1,outcome_cols)]

#binary cols
#binary_cols = which(unlist(sapply(multi_morbid, function(x) length(levels(factor(x)))==2)))

# sapply(mydata[,binary_cols],function (x) prop.table(table(x)))
#symm_cols = binary_cols[names(binary_cols)!='gender']

#manhattan distance
man.dist = daisy(multi_morbid, metric = 'manhattan')

saveRDS(man.dist,'results/distance_matrix/manhattan_distance_multi_morbid.rds')
