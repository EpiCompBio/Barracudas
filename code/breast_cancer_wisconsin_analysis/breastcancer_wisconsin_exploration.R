rm(list=ls())


################################################################################################################
# LOADING PACKAGES
################################################################################################################

using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using("ggplot2","FactoMineR","reshape2","cluster","dbscan","keras","apcluster","clusterCrit","kernlab",
      "randomForest","mclust")

############################################################
# DEFINE THEMES & SOURCING FUNCTIONS
############################################################

theme_jh = theme_grey() + theme(plot.title=element_text(family="serif",face="bold",color="#8d0e00",hjust=0.5,vjust=0.5,size=16)) +
  theme(panel.background = element_rect(fill='#f9f9f9', colour='white')) +
  theme(axis.title = element_text(family="serif",face="bold",size=12,hjust=0.5)) +
  theme(axis.line = element_line(colour = "#747474",arrow=arrow(angle = 30,length = unit(0.10, "inches")))) +
  theme(legend.background = element_rect(fill='#f9f9f9', colour='black')) + 
  theme(legend.text = element_text(family="serif",hjust=0.5)) + 
  theme(legend.title = element_text(family="serif",face="bold",hjust=0.5)) +
  theme(legend.key.size = unit(1,"line")) +
  theme(plot.margin=unit(c(.5, .5, .5, .5), "cm"))

path<-dirname(rstudioapi::getActiveDocumentContext()$path)

path.functions = paste0(path,'/utility_functions_R/')

source(paste0(path.functions,"PCA_plots_utility.R"))
source(paste0(path.functions,"clustering_utility.R"))
source(paste0(path.functions,"colors_themes_utility.R"))
source(paste0(path.functions,"standard_plots_ggplot2_utility.R"))
################################################################################################################
# LOADING THE DATA
################################################################################################################

#Some description of the variables 

# 1) ID number
# 2) Diagnosis (M = malignant, B = benign)
# 3-32) Ten real-valued features are computed for each cell nucleus:
#   a) radius (mean of distances from center to points on the perimeter)
# b) texture (standard deviation of gray-scale values)
# c) perimeter
# d) area
# e) smoothness (local variation in radius lengths)
# f) compactness (perimeter^2 / area - 1.0)
# g) concavity (severity of concave portions of the contour)
# h) concave points (number of concave portions of the contour)
# i) symmetry 
# j) fractal dimension ("coastline approximation" - 1)

breast_cancer=read.table("breastCancer_wisconsin.txt", sep=",")

breast_cancer_clustering=breast_cancer[,3:ncol(breast_cancer)]
breast_cancer_clustering=as.data.frame(scale(breast_cancer_clustering))

################################################################################################################
# PCA
################################################################################################################

PCA_breast_cancer_clustering=PCA(breast_cancer_clustering,ncp = 5, graph = FALSE)

################################################################################################################
# Auto-encoder => A Neural network for dimensionality reduction
################################################################################################################

##########################
# Functional approach  

set.seed(2)
Input = layer_input(shape=ncol(breast_cancer_clustering))
encoder_raw_1 = Input %>% layer_dense(units=24)  
encoder_activation_1 = encoder_raw_1 %>% layer_activation(activation="relu",name="encoder_activatio1")
encoder_raw_2 = encoder_activation_1 %>% layer_dense(units=16)  
encoder_activation_2 = encoder_raw_2 %>% layer_activation(activation="relu",name="encoder_activation2")
encoder_raw_3 = encoder_activation_2 %>% layer_dense(units=10)  
encoder_activation_3 = encoder_raw_3 %>% layer_activation(activation="relu",name="encoder_activation3")
decoder_raw = encoder_activation_3 %>% layer_dense(units=ncol(breast_cancer_clustering)) 
decoder_activation = decoder_raw %>% layer_activation(activation="linear")

#Define the model with input and output layers
model <- keras_model(inputs = Input, outputs = decoder_activation)


# create and compile model
model %>% compile(
  optimizer = 'adadelta',
  loss = 'mean_squared_error',
  metrics = c('mae')
)


# fit the model
model %>% fit(
  as.matrix(breast_cancer_clustering), as.matrix(breast_cancer_clustering),
  epochs = 1000,
  validation_split = 0,
  verbose=0
)

###################
#Model evaluation on the training set
evaluation_score <- model %>% evaluate(as.matrix(breast_cancer_clustering),
                                       as.matrix(breast_cancer_clustering))

print(evaluation_score)


##############################
# Looking at the hidden layer 

layer_name <- "encoder_activation3"
intermediate_layer_model <- keras_model(inputs = model$input,
                                        outputs = get_layer(model, layer_name)$output)
intermediate_output <- predict(intermediate_layer_model, as.matrix(breast_cancer_clustering)) #This corresponds to the new scores 
#of the datapoints in the smaller space

################################################################################################################
################################################################################################################
# Kmeans
################################################################################################################
################################################################################################################

n_centers=seq(2:10)
tot_withinss=rep(0,length(n_centers))


# Different numbers of centers
for (k in 1:length(n_centers)) {
  
breast_cancer_kmeans=kmeans(breast_cancer_clustering, center = n_centers[k])
tot_withinss[k]=breast_cancer_kmeans$tot.withinss


}

########################
# Elbow plot
########################

elbow_plot_kmeans = ggplot(data=data.frame(n_centers,tot_withinss)) + geom_point(aes(x=n_centers,y=tot_withinss)) + theme_jh
x11()
print(elbow_plot_kmeans)

#Best is 2 ?
kmeans_breast_cancer_clustering = as.factor(kmeans(breast_cancer_clustering, center = 2)$cluster)

#Plotting the individuals colored by cluster on the first two PCA components
kmeans_breast_cancer_plots = make_PCA_ind_plot_classes(PCA_breast_cancer_clustering,dims=c(1,2),classes=as.factor(kmeans_breast_cancer_clustering),
                          custom_theme=theme_jh,color_scale=distinct_scale,
                          write_labels=TRUE)
x11()
print(kmeans_breast_cancer_plots)

#Is it meaningful given the classes ?
table(kmeans_breast_cancer_clustering,breast_cancer$V2)


mean_clusters_plot=mean_by_cluster_continuous(breast_cancer_clustering,classes = as.factor(kmeans_breast_cancer_clustering),
                           color_scale= distinct_scale, custom_theme= theme_jh)

x11()
print(mean_clusters_plot)


########################
# Parangon individuals 
########################

parangon_indviduals_list=rep(0,length(levels(kmeans_breast_cancer_clustering)))

for (i in 1:length(levels(kmeans_breast_cancer_clustering))) {

  parangon_indviduals_list[i]=parangon(breast_cancer_clustering[kmeans_breast_cancer_clustering==levels(kmeans_breast_cancer_clustering)[i],])
}

# An individual that represents the first cluster very well
print(breast_cancer_clustering[434,])



#Silhouette plot
silhouette_plot_kmeans=silhouette_plot_ggplot2(breast_cancer_clustering,classes=kmeans_breast_cancer_clustering,
                        daisy_metric="euclidean",
                        custom_theme=theme_jh,color_scale=distinct_scale,title=NULL)
x11()
print(silhouette_plot_kmeans)

intCriteria(as.matrix(breast_cancer_clustering),as.integer(as.character(kmeans_breast_cancer_clustering)),"Calinski_Harabasz")

################################################################################################################
################################################################################################################
# Spectral Clustering 
################################################################################################################
################################################################################################################



spectral_clustering_breast_cancer <- specc(as.matrix(breast_cancer_clustering), centers=2, kernel = "polydot",kpar=list(degree=2))

spectral_clustering_breast_cancer_plot = make_PCA_ind_plot_classes(PCA_breast_cancer_clustering,dims=c(1,2),classes=as.factor(spectral_clustering_breast_cancer),
                                                       custom_theme=theme_jh,color_scale=distinct_scale,
                                                       write_labels=TRUE)

x11()
print(spectral_clustering_breast_cancer_plot )

intCriteria(as.matrix(breast_cancer_clustering),as.integer(as.character(spectral_clustering_breast_cancer)),"Calinski_Harabasz")


################################################################################################################
################################################################################################################
# DBSCAN
################################################################################################################
################################################################################################################

dbscan_clustering_breast_cancer <- dbscan(as.matrix(breast_cancer_clustering), eps = 4)



dbscan_clustering_breast_cancer_plot = make_PCA_ind_plot_classes(PCA_breast_cancer_clustering,dims=c(1,2),
                                                                   classes=as.factor(dbscan_clustering_breast_cancer$cluster),
                                                                   custom_theme=theme_jh,color_scale=distinct_scale,
                                                                   write_labels=TRUE)

x11()
print(dbscan_clustering_breast_cancer_plot )


intCriteria(as.matrix(breast_cancer_clustering),as.integer(as.character(dbscan_clustering_breast_cancer$cluster)),"Calinski_Harabasz")

################################################################################################################
################################################################################################################
# Kmeans on Auto-Encoder
################################################################################################################
################################################################################################################


kmeans_autoencoder=kmeans(intermediate_output,center=2)


autoencoder_kmeans_clustering_breast_cancer_plot = make_PCA_ind_plot_classes(PCA_breast_cancer_clustering,dims=c(1,2),
                                                                             classes=as.factor(kmeans_autoencoder$cluster),
                                                                             custom_theme=theme_jh,color_scale=distinct_scale,
                                                                             write_labels=TRUE)
x11()
print(autoencoder_kmeans_clustering_breast_cancer_plot)


print(table(kmeans_autoencoder$cluster,breast_cancer$V2))

intCriteria(as.matrix(breast_cancer_clustering),as.integer(as.character(kmeans_autoencoder$cluster)),"Calinski_Harabasz")


################################################################################################################
################################################################################################################
# Affinity propagation
################################################################################################################
################################################################################################################


affinity_prop_clustering_breast_cancer <- apclusterK(negDistMat(r=2), breast_cancer_clustering,K=2, details=TRUE)

affinity_prop_clusters=rep(0,nrow(breast_cancer_clustering))


for (k in 1:length(affinity_prop_clustering_breast_cancer@clusters)) {
  for (i in 1:length(affinity_prop_clustering_breast_cancer@clusters[[k]])) {
    
    affinity_prop_clusters[affinity_prop_clustering_breast_cancer@clusters[[k]][i]]=k
    
  }
}


affinity_prop_clustering_breast_cancer_plot = make_PCA_ind_plot_classes(PCA_breast_cancer_clustering,dims=c(1,2),
                                                                 classes=as.factor(affinity_prop_clusters),
                                                                 custom_theme=theme_jh,color_scale=distinct_scale,
                                                                 write_labels=TRUE)


x11()
print(affinity_prop_clustering_breast_cancer_plot)

intCriteria(as.matrix(breast_cancer_clustering),as.integer(as.character(affinity_prop_clusters)),"Calinski_Harabasz")



################################################################################################################
################################################################################################################
# Random Forests Similarity matrix + Partition around Medoids
################################################################################################################
################################################################################################################

RF_proximity_measures=randomForest(x=breast_cancer_clustering)$proximity
pam_clustering_RF=pam(RF_proximity_measures, 2)


pam_clustering_RF_breast_cancer_plot = make_PCA_ind_plot_classes(PCA_breast_cancer_clustering,dims=c(1,2),
                                                                        classes=as.factor(pam_clustering_RF$clustering),
                                                                        custom_theme=theme_jh,color_scale=distinct_scale,
                                                                        write_labels=TRUE)


x11()
print(pam_clustering_RF_breast_cancer_plot)

intCriteria(as.matrix(breast_cancer_clustering),as.integer(as.character(pam_clustering_RF$clustering)),"Calinski_Harabasz")


################################################################################################################
################################################################################################################
# Gaussian Mixture Models
################################################################################################################
################################################################################################################



gaussian_mixture_clustering  <- Mclust(breast_cancer_clustering, 2)

gaussian_mixture_clustering_breast_cancer_plot = make_PCA_ind_plot_classes(PCA_breast_cancer_clustering,dims=c(1,2),
                                                                 classes=as.factor(gaussian_mixture_clustering$classification),
                                                                 custom_theme=theme_jh,color_scale=distinct_scale,
                                                                 write_labels=TRUE)


x11()
print(gaussian_mixture_clustering_breast_cancer_plot)

intCriteria(as.matrix(breast_cancer_clustering),as.integer(as.character(gaussian_mixture_clustering$classification)),"Calinski_Harabasz")
# Note that for most criteria, just like for DBSCAN won't make much sense here