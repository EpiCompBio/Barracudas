library(tableone)
setwd('~/Documents/Translational/')
mydata = read.csv('data/processed/UKBcompleteFeb19.csv')

#change gender levels
mydata$Sex = ifelse(mydata$gender == 0, 'Female','Male')

#define morbidly obese
mydata$obese = ifelse(mydata$BMI >= 35, 1, 0)

#define outcome cols
outcomes = c('diabetes','mi','stroke','obese','angina')

outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(mydata))

#col of chronic diseases
mydata$no_chronic = apply(mydata[,outcome_cols],1,sum)

#binary cols
binary_cols = which(unlist(sapply(mydata, function(x) length(levels(factor(x)))==2)))

#change binary col class
mydata[,binary_cols] = sapply(mydata[,binary_cols], factor)

#subset multi morbid rows
# multi_morbid = mydata[which(mydata$no_chronic>1),]

vars <- c('birth_year',
          'BMI',
          'current_smoker',
          'current_etoh',
          'mi',
          'angina',
          'stroke',
          'diabetes',
          'obese')
catVars <- c('current_smoker',
             'current_etoh',
             'mi',
             'angina',
             'stroke',
             'diabetes',
             'obese')
strata <- c('Sex',
            'no_chronic'#,
            # 'mi',
            # 'angina',
            # 'stroke',
            # 'diabetes',
            #'obese'
            )
table1 <- CreateTableOne(vars = vars,
                         strata = strata,
                         factorVars = catVars, data = mydata)
Table1Matrix <- print(table1, quote=FALSE, noSpaces = TRUE, contDigits=1)
write.csv(Table1Matrix,'Barracudas/descriptive_analysis_results/tableone.csv')
summary(table1)
