library(tableone)
setwd('~/Documents/Translational/')
mydata = read.csv('data/processed/UKBcompleteFeb19.csv')

#change gender levels
mydata$Sex = ifelse(mydata$gender == 0, 'Female','Male')

#define outcome cols
outcomes = c('diabetes','mi','stroke','dvt_asthma_copd_atopy','angina')

outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(mydata))

#col of chronic diseases
mydata$no_chronic = apply(mydata[,outcome_cols],1,sum)

#subset multi morbid rows
multi_morbid = mydata[which(mydata$no_chronic>1),]

#binary cols
binary_cols = which(unlist(sapply(multi_morbid, function(x) length(levels(factor(x)))==2)))

#change binary col class
multi_morbid[,binary_cols] = sapply(multi_morbid[,binary_cols], factor)

vars <- c('birth_year',
          'BMI',
          'current_smoker',
          'current_etoh',
          'mi',
          'angina',
          'stroke',
          'dvt_asthma_copd_atopy',
          'diabetes')
catVars <- c('current_smoker',
             'current_etoh',
             'mi',
             'angina',
             'stroke',
             'dvt_asthma_copd_atopy',
             'diabetes')
strata <- c('Sex',
            'no_chronic'#,
            # 'mi',
            # 'angina',
            # 'stroke',
            # 'dvt_asthma_copd_atopy',
            # 'diabetes'
            )
table1 <- CreateTableOne(vars = vars,
                         strata = strata,
                         factorVars = catVars, data = multi_morbid )
Table1Matrix <- print(table1, quote=FALSE, noSpaces = TRUE, contDigits=1)
write.csv(Table1Matrix,'Barracudas/descriptive_analysis/tableone.csv')
summary(table1)
