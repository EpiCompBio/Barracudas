setwd('~/Documents/Translational/results/clustering/')
cluster_names = list.files()
clusters = list()
for (i in 1:length(cluster_names)){
clusters[[i]] = readRDS(cluster_names[i])
}
names(clusters) = substr(cluster_names,1,nchar(cluster_names)-4)

cut_2 = cutree(diana_manhattan_multi_morbid, k = 2)
cut_3 = cutree(diana_manhattan_multi_morbid, k = 3)
cut_4 = cutree(diana_manhattan_multi_morbid, k = 4)
cut_5 = cutree(diana_manhattan_multi_morbid, k = 5)
table(cut_2)
table(cut_3)
table(cut_4)
table(cut_5)
