#lib.path = '/rds/general/user/je108/home/anaconda3/lib/R/library/'

suppressPackageStartupMessages(library(doParallel #, lib.loc = lib.path
))

#setwd('/rds/general/project/medbio-berlanga-group/live/projects/group_multi_morbidity/results/clustering')

setwd('~/Documents/Translational/results/clustering')

gower_cluster_summary = readRDS('summaries/gower_cluster_summary.rds')

gower_cophenetic = readRDS('summaries/gower_cophenetic.rds')

gower_diana = readRDS('diana_gower_multi_morbid.rds')

gower.dist = readRDS('../distance_matrix/gower_distance_multi_morbid.rds')

#compile list of clustering objects
cluster_files = list.files(pattern = 'gower')
clusters = list()
for (i in 1:length(cluster_files)){
clusters[[i]] = readRDS(cluster_files[i])
}
cluster_names = substr(cluster_files,1,nchar(cluster_files)-17)
names(clusters) = cluster_names

# clusters cutree for each method
clusters_2 = list()
clusters_3 = list()
clusters_4 = list()
for (i in 1:length(cluster_names)){
clusters_2[[i]] = cutree(clusters[[i]],k=2)
clusters_3[[i]] = cutree(clusters[[i]],k=3)
clusters_4[[i]] = cutree(clusters[[i]],k=4)
}
names(clusters_2) = cluster_names
names(clusters_3) = cluster_names
names(clusters_4) = cluster_names

# tables of clusters per group --  2 clusters
cluster_2_summary = data.frame(NULL)
for (i in 1:length(cluster_names)){
  cluster_2_summary = rbind(cluster_2_summary, table(clusters_2[i]))
}
cluster_2_summary = cbind(cluster_2_summary,cluster_names)
names(cluster_2_summary) = c('Cluster 1','Cluster 2','Model')

# tables of clusters per group --  3 clusters
cluster_3_summary = data.frame(NULL)
for (i in 1:length(cluster_names)){
  cluster_3_summary = rbind(cluster_3_summary, table(clusters_3[i]))
}
cluster_3_summary = cbind(cluster_3_summary,cluster_names)
names(cluster_3_summary) = c('Cluster 1','Cluster 2','Cluster 3','Model')

# tables of clusters per group --  4 clusters
cluster_4_summary = data.frame(NULL)
for (i in 1:length(cluster_names)){
  cluster_4_summary = rbind(cluster_4_summary, table(clusters_4[i]))
}
cluster_4_summary = cbind(cluster_4_summary,cluster_names)
names(cluster_4_summary) = c('Cluster 1','Cluster 2','Cluster 3','Cluster 4','Model')

#cophenetic distances for dendrograms -- gower
cl = makeCluster(6)
registerDoParallel(cl)
cor.cophenetic = foreach(i = 1:length(clusters), .combine = rbind) %dopar% {
  d.cophenetic = cophenetic(as.hclust(clusters[[i]]))
  cor.cophen = c(cor(gower.dist,d.cophenetic),cluster_names[i])
  return(cor.cophen)
}
colnames(cor.cophenetic) = c('cor_cophenetic','model')
saveRDS(cor.cophenetic,'gower_cophenetic.rds')

gower_cluster_summary = list(NULL)
gower_cluster_summary$two = cluster_2_summary
gower_cluster_summary$three = cluster_3_summary
gower_cluster_summary$four = cluster_4_summary

saveRDS(gower_cluster_summary,'gower_cluster_summary.rds')