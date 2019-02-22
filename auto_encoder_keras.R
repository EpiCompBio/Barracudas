################################################################################################################
# LOADING THE LIBRARIES
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

using("ggplot2","FactoMineR","reshape2","keras")

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

################################################################################################################
# LOADING THE DATA
################################################################################################################

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

setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/TDS_Github")
breast_cancer=read.table("breastCancer_wisconsin.txt", sep=",")

breast_cancer_clustering=breast_cancer[,3:ncol(breast_cancer)]
breast_cancer_clustering=scale(breast_cancer_clustering)


########################################################################
# Functional approach  
########################################################################


Input = layer_input(shape=ncol(breast_cancer_clustering))
encoder_raw_1 = Input %>% layer_dense(units=24)  
encoder_activation_1 = encoder_raw_1 %>% layer_activation(activation="relu",name="encoder_activatio1")
encoder_raw_2 = encoder_activation_1 %>% layer_dense(units=16)  
encoder_activation_2 = encoder_raw_2 %>% layer_activation(activation="relu",name="encoder_activation2")
encoder_raw_3 = encoder_activation_2 %>% layer_dense(units=8)  
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
  breast_cancer_clustering, breast_cancer_clustering,
  epochs = 1000,
  validation_split = 0,
  verbose=0
)

###################
#Model evaluation on the training set
evaluation_score <- model %>% evaluate(breast_cancer_clustering,
                                       breast_cancer_clustering)

print(evaluation_score)


####################################################################################
# Looking at the hidden layer 
####################################################################################

layer_name <- "encoder_activation3"
intermediate_layer_model <- keras_model(inputs = model$input,
                                        outputs = get_layer(model, layer_name)$output)
intermediate_output <- predict(intermediate_layer_model, breast_cancer_clustering)


test=kmeans(intermediate_output,center=3)
print(table(test$cluster,breast_cancer$V2))

#Doesn't work because I didn't upload the functions
# autoencoder_kmeans_clustering_breast_cancer_plot = make_PCA_ind_plot_classes(PCA_breast_cancer_clustering,dims=c(1,2),
#                                                                  classes=as.factor(test$cluster),
#                                                                  custom_theme=theme_jh,color_scale=distinct_scale,
#                                                                  write_labels=TRUE)
# x11()
# print(autoencoder_kmeans_clustering_breast_cancer_plot)
# 




