library(ggplot2)
library(dplyr)
library(tidyr)
library(scales) 
setwd('~/Documents/Translational/')
mydata = read.csv('data/processed/UKBcompleteFeb19.csv')

#define obese BMI > 35
mydata$obese = ifelse(mydata$BMI >= 35, 1, 0)

#define outcome cols
outcomes = c('diabetes','mi','stroke','angina','obese')

outcome_cols = grep(paste0('^',outcomes,'$',collapse = '|'), colnames(mydata))

#col of chronic diseases
mydata$no_chronic = apply(mydata[,outcome_cols],1,sum)

#change gender levels
mydata$Sex = factor(ifelse(mydata$gender == 0, 'Female','Male'))

#binary cols
binary_cols = which(unlist(sapply(mydata, function(x) length(levels(factor(x)))==2)))

#change binary col class
mydata[,binary_cols] = sapply(mydata[,binary_cols], factor)

#subset multi morbid rows
multi_morbid = mydata[which(mydata$no_chronic>1),]

#Birth year density plot
svg('Barracudas/plots/birth_years.svg',width=10,height=10)

ggplot(mydata, aes(x = birth_year, fill = Sex)) +
  geom_density(alpha=0.3, aes(fill = Sex)) + facet_wrap(~ no_chronic)+
  scale_fill_brewer(palette="Dark2") +
  labs(title = 'Number of chronic diseases',
       x = 'Birth year',
       y = 'Density') +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

dev.off()

#BMI density plot
svg('Barracudas/plots/BMI.svg',width=10,height=10)

ggplot(mydata, aes(x = BMI, fill = Sex)) +
  geom_density(alpha=0.3, aes(fill = Sex)) + facet_wrap(~ no_chronic)+
  scale_fill_brewer(palette="Dark2") +
  labs(title = 'Number of chronic diseases',
       x = 'BMI',
       y = 'Density') +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

dev.off()

#alcohol bar plot
svg('Barracudas/plots/alcohol.svg',width=10,height=10)

ggplot(mydata, aes(x = current_etoh, fill = Sex)) +
  geom_bar(aes(fill = Sex)) + facet_wrap(~ no_chronic)+
  scale_fill_brewer(palette="Dark2") +
  labs(title = 'Number of chronic diseases',
       x = 'Current Alcohol Consumption',
       y = 'Count') +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

dev.off()


#smoking bar plot
svg('Barracudas/plots/smoker.svg',width=10,height=10)

ggplot(mydata, aes(x = current_smoker, fill = Sex)) +
  geom_bar(aes(fill = Sex)) + facet_wrap(~ no_chronic)+
  scale_fill_brewer(palette="Dark2") +
  labs(title = 'Number of chronic diseases',
       x = 'Current Smoker',
       y = 'Count') +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

dev.off()