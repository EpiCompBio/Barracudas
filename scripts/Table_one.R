library(tableone)
setwd('~/Documents/Translational/')
mydata = read.csv('data/processed/UKBcompleteFeb19.csv')
vars <- c('birth_year',
          'BMI',
          'gender',
          'current_smoker',
          'current_etoh',
          'mi',
          'angina',
          'stroke',
          'dvt_asthma_copd_atopy',
          'diabetes')
catVars <- c('current_smoker',
             'current_etoh')
strata <- c('gender',
            'mi',
            'angina',
            'stroke',
            'dvt_asthma_copd_atopy',
            'diabetes')
table1 <- CreateTableOne(vars = vars, strata = strata, factorVars = catVars, data = mydata )
Table1Matrix <- print(table1, quote=FALSE, noSpaces = TRUE, contDigits=1)
write.csv(Table1Matrix,'tableone.csv')
summary(table1)
