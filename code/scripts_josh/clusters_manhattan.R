lib.path = '/rds/general/user/je108/home/anaconda3/lib/R/library/'

suppressPackageStartupMessages(library(cluster, lib.loc = lib.path
))

setwd('/rds/general/project/medbio-berlanga-group/live/projects/group_multi_morbidity/')

man.dist = readRDS('results/distance_matrix/manhattan_distance_multi_morbid.rds')

#DIANA on manhattan dist
res.diana.manhattan = diana(man.dist, diss = TRUE, keep.diss = F, keep.data = F)
saveRDS(res.diana.manhattan,'results/clustering/diana_manhattan_multi_morbid.rds')

#complete linkage agg hierarchical
res.complete.manhattan = hclust(man.dist, method = 'complete')
saveRDS(res.complete.manhattan,'results/clustering/complete_linkage_manhattan_multi_morbid.rds')

#average linkage agg hierarchical
res.average.manhattan = hclust(man.dist, method = 'average')
saveRDS(res.average.manhattan,'results/clustering/average_linkage_manhattan_multi_morbid.rds')

#ward linkage agg hierarchical
res.ward.manhattan = hclust(man.dist, method = 'ward.D')
saveRDS(res.ward.manhattan,'results/clustering/ward_D_linkage_manhattan_multi_morbid.rds')

#centroid linkage agg hierarchical
res.centroid.manhattan = hclust(man.dist, method = 'centroid')
saveRDS(res.centroid.manhattan,'results/clustering/centroid_linkage_manhattan_multi_morbid.rds')

#single linkage agg hierarchical
res.single.manhattan = hclust(man.dist, method = 'single')
saveRDS(res.single.manhattan,'results/clustering/single_linkage_manhattan_multi_morbid.rds')