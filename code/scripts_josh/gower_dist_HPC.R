lib.path = '/rds/general/user/je108/home/anaconda3/lib/R/library/'

suppressPackageStartupMessages(library(cluster, lib.loc = lib.path
))

setwd('/rds/general/project/medbio-berlanga-group/live/projects/group_multi_morbidity/')

mydata = read.csv('data/processed/UKBcompleteFeb19.csv',header=T)

#define obese BMI > 35
mydata$obese = ifelse(mydata$BMI >= 35, 1, 0)

#define outcome col names andn dataset
outcomes = c('diabetes','mi','stroke','angina','obese')

outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(mydata))

#col of chronic diseases
no_chronic = apply(mydata[,outcome_cols],1,sum)

#predictors
multi_morbid = mydata[which(no_chronic>1),-c(1,outcome_cols)]

#binary cols
binary_col_ids = which(unlist(sapply(multi_morbid, function(x) length(levels(factor(x)))==2)))

#symmetrical binary cols
symm_cols = binary_col_ids[names(binary_col_ids)!='gender']

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
multi_morbid[,-c(binary_col_ids,cat_col_ids,ord_col_ids)] = as.data.frame(scale(multi_morbid[,-c(binary_col_ids,cat_col_ids,ord_col_ids)]))

#gower distance
gower.dist = daisy(multi_morbid, metric = 'gower',
                   type = list(asymm = 'gender', symm = symm_cols))

saveRDS(gower.dist,'results/distance_matrix/gower_distance_multi_morbid.rds')
