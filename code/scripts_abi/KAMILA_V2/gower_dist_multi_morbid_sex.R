#lib.path = '/rds/general/user/je108/home/anaconda3/lib/R/library/'

suppressPackageStartupMessages(library(cluster#, lib.loc = lib.path
))
library(KODAMA)

#setwd('/rds/general/project/medbio-berlanga-group/live/projects/group_multi_morbidity/')
setwd('~/Documents/Translational/')

mydata = read.csv('data/processed/UKBcompleteFeb19.csv',header=T)

#define obese BMI > 35
mydata$obese = ifelse(mydata$BMI >= 40, 1, 0)

#define outcome col names andn dataset
outcomes = c('diabetes','mi','stroke','angina','obese')

#outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(mydata))

#col of chronic diseases
no_chronic = apply(mydata[,outcomes],1,sum)
#mydata$number_chronic = no_chronic

#predictors
remove_cols = grep(paste0(paste0('^',outcomes,'$',collapse = '|'),'|eid|dvt_asthma_copd_atopy|htn'), colnames(mydata))

multi_morbid = mydata[which(no_chronic>1),-remove_cols]

# data set by sex
females = multi_morbid[multi_morbid$gender == 0,-c('gender')]
males = multi_morbid[multi_morbid$gender == 1,-c('gender')]
# frequency sex/age match 1:1 case:control

all_females = mydata[mydata$gender == 0,-c(remove_cols,grep('^gender$',colnames(mydata)))]
set.seed(1)
control_females = all_females[sample(nrow(all_females),nrow(females)),]

all_males = mydata[mydata$gender == 1,-c(remove_cols,grep('^gender$',colnames(mydata)))]
set.seed(1)
control_males = all_males[sample(nrow(all_males),nrow(males)),]


matched_females = frequency_matching(all_females[,"birth_year"],
                                     all_females$number_chronic >1,times=1,seed=1)

matched = as.integer(matched$selection)

matched_controls = mydata[intersect(matched,which(no_chronic<2)),]

### === gower dist ===== ####
gower_dist_sex = function(dataset){

#binary cols
symm_cols = which(unlist(sapply(dataset, function(x) length(levels(factor(x)))==2)))

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

cat_col_ids = which(colnames(dataset) %in% cat_cols)
ord_col_ids = which(colnames(dataset) %in% ord_cols) 

dataset[,cat_col_ids] = factor(dataset[,cat_col_ids])
dataset[,ord_col_ids] = lapply(dataset[,ord_col_ids], function(x) factor(as.integer(x), ordered = TRUE))

#scale numeric features
dataset[,-c(symm_cols,cat_col_ids,ord_col_ids)] = as.data.frame(scale(dataset[,-c(symm_cols,cat_col_ids,ord_col_ids)]))

#gower distance
gower.dist = daisy(dataset, metric = 'gower',
                   type = list(symm = symm_cols))
return(gower.dist)}

males_gower = gower_dist_sex(males)
females_gower = gower_dist_sex(females)

saveRDS(females_gower,'results/distance_matrix/gower_distance_cases_females.rds')
