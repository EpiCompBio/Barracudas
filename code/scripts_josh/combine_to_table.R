library(ggplot2)
setwd('~/Documents/Translational/results/clustering/summaries')

#gower
gower_names = list.files(pattern = 'gower')
gower_four_clusters = readRDS(gower_names[1])$four
gower_cophenetic = data.frame(readRDS(gower_names[2]))
gower_four_clusters$cophenetic_correlation = as.numeric(as.character(gower_cophenetic$cor_cophenetic))

gower_four_clusters$Model = factor(gower_four_clusters$Model,
                                   levels = gower_four_clusters$Model[order(gower_four_clusters$cophenetic_correlation)])
gower_four_clusters$distance = 'Gower'

#manhattan
manhattan_names = list.files(pattern = 'manhattan')
manhattan_four_clusters = readRDS(manhattan_names[1])$four
manhattan_cophenetic = data.frame(readRDS(manhattan_names[2]))
manhattan_four_clusters$cophenetic_correlation = as.numeric(as.character(manhattan_cophenetic$cor_cophenetic))

manhattan_four_clusters$Model = factor(manhattan_four_clusters$Model,
                                   levels = manhattan_four_clusters$Model[order(manhattan_four_clusters$cophenetic_correlation)])

manhattan_four_clusters$distance = 'Manhattan'

#combine
combined = rbind(gower_four_clusters,manhattan_four_clusters)
combined$distance = factor(combined$distance)

# plot
ggplot(combined, aes(x=Model,y=cophenetic_correlation, col = distance)) + geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(y = 'Cophenetic Correlation', col = 'Distance Metric')




manhattan_four_clusters = readRDS(manhattan_names[1])$four

manhattan_cophenetic = readRDS(manhattan_names[2])


