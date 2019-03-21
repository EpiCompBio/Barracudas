#lib.path = '/rds/general/user/je108/home/anaconda3/lib/R/library/'

suppressPackageStartupMessages(library(cluster#, lib.loc = lib.path
))
library(KODAMA)

#setwd('/rds/general/project/medbio-berlanga-group/live/projects/group_multi_morbidity/')
setwd('~/Documents/Translational/')

mydata = read.csv('data/processed/UKBcompleteFeb19.csv',header=T)

#define obese BMI > 35
mydata$obese = ifelse(mydata$BMI >= 35, 1, 0)

#define outcome col names andn dataset
outcomes = c('diabetes','mi','stroke','angina','obese')

#outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(mydata))

#col of chronic diseases
no_chronic = apply(mydata[,outcomes],1,sum)
mydata$number_chronic = no_chronic

#predictors
remove_cols = grep(paste0(paste0('^',outcomes,'$',collapse = '|'),'|eid|dvt_asthma_copd_atopy|htn'), colnames(mydata))

multi_morbid = mydata[which(no_chronic>1),-remove_cols]

# frequency sex/age match 1:1 case:control

matched = frequency_matching(mydata[, c("birth_year","gender")],
                             mydata$number_chronic >1,times=1,seed=1)

matched = as.integer(matched$selection)

matched_controls = mydata[intersect(matched,which(no_chronic<2)),]

#binary cols
binary_col_controls_ids = which(unlist(sapply(matched_controls, function(x) length(levels(factor(x)))==2)))

#symmetrical binary cols
symm_controls_cols = binary_col_ids[names(binary_col_controls_ids)!='gender']

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

cat_col_ids = which(colnames(matched_controls) %in% cat_cols)
ord_col_ids = which(colnames(matched_controls) %in% ord_cols) 

matched_controls[,cat_col_ids] = factor(matched_controls[,cat_col_ids])
matched_controls[,ord_col_ids] = lapply(matched_controls[,ord_col_ids], function(x) factor(as.integer(x), ordered = TRUE))

#scale numeric features
matched_controls[,-c(binary_col_controls_ids,cat_col_ids,ord_col_ids)] = as.data.frame(scale(matched_controls[,-c(binary_col_controls_ids,cat_col_ids,ord_col_ids)]))

#gower distance
gower.dist = daisy(matched_controls, metric = 'gower',
                   type = list(asymm = 'gender', symm = symm_controls_cols))

saveRDS(gower.dist,'results/distance_matrix/gower_distance_multi_morbid.rds')
