lib.path = '/rds/general/user/je108/home/anaconda3/lib/R/library/'

suppressPackageStartupMessages(library(cluster, lib.loc = lib.path
))

setwd('/rds/general/project/medbio-berlanga-group/live/projects/group_multi_morbidity/')

gower.dist = readRDS('results/distance_matrix/gower_distance_controls.rds')

#DIANA on gower dist
res.diana.gower = diana(gower.dist, diss = TRUE, keep.diss = F, keep.data = F)
saveRDS(res.diana.gower,'results/clustering/diana_gower_controls.rds')

#complete linkage agg hierarchical
res.complete.gower = hclust(gower.dist, method = 'complete')
saveRDS(res.complete.gower,'results/clustering/complete_linkage_gower_controls.rds')

#average linkage agg hierarchical
res.average.gower = hclust(gower.dist, method = 'average')
saveRDS(res.average.gower,'results/clustering/average_linkage_gower_controls.rds')

#ward linkage agg hierarchical
res.ward.gower = hclust(gower.dist, method = 'ward.D')
saveRDS(res.ward.gower,'results/clustering/ward_D_linkage_gower_controls.rds')

#centroid linkage agg hierarchical
res.centroid.gower = hclust(gower.dist, method = 'centroid')
saveRDS(res.centroid.gower,'results/clustering/centroid_linkage_gower_controls.rds')

#single linkage agg hierarchical
res.single.gower = hclust(gower.dist, method = 'single')
saveRDS(res.single.gower,'results/clustering/single_linkage_gower_controls.rds')