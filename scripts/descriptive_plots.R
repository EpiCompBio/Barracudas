library(ggplot2)
setwd('~/Documents/Translational/')
mydata = read.csv('data/processed/UKBcompleteFeb19.csv')

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

#change gender levels
multi_morbid$Sex = ifelse(multi_morbid$gender == 0, 'Female','Male')

#Birth year density plot
png('Barracudas/plots/birth_years.png',width=1000,height=1000)

ggplot(multi_morbid, aes(x = birth_year, fill = Sex)) +
  geom_density(alpha=0.3, aes(fill = gender)) + facet_wrap(~ no_chronic)+
  scale_fill_brewer(palette="Dark2") +
  labs(title = 'Number of chronic diseases',
            x = 'Birth year',
            y = 'Density') +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
                              
dev.off()

#BMI density plot
png('Barracudas/plots/BMI.png',width=1000,height=1000)

ggplot(multi_morbid, aes(x = BMI, fill = Sex)) +
  geom_density(alpha=0.3, aes(fill = gender)) + facet_wrap(~ no_chronic)+
  scale_fill_brewer(palette="Dark2") +
  labs(title = 'Number of chronic diseases',
       x = 'BMI',
       y = 'Density') +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

dev.off()

#alcohol bar plot
png('Barracudas/plots/alcohol.png',width=1000,height=1000)

ggplot(multi_morbid, aes(x = current_etoh, fill = Sex)) +
  geom_bar(aes(fill = gender)) + facet_wrap(~ no_chronic)+
  scale_fill_brewer(palette="Dark2") +
  labs(title = 'Number of chronic diseases',
       x = 'Current Alcohol Consumption',
       y = 'Count') +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

dev.off()


#smoking bar plot
png('Barracudas/plots/smoker.png',width=1000,height=1000)

ggplot(multi_morbid, aes(x = current_smoker, fill = Sex)) +
  geom_bar(aes(fill = gender)) + facet_wrap(~ no_chronic)+
  scale_fill_brewer(palette="Dark2") +
  labs(title = 'Number of chronic diseases',
       x = 'Current Smoker',
       y = 'Count') +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

dev.off()
