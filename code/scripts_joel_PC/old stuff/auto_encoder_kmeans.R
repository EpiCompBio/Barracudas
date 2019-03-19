#########
# THIS WILL NOT BE USED SINCE THE REPRESENTATION OF THE EXAMPLE DATA IS EXTREMELY UNSTABLE
#########


################################################################################
# LOADING LIBRARIES
################################################################################

using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using("keras","caret")

################################################################################
# WORKING DIRECTORY AND SOURCING FUNCTIONS
################################################################################

file_path<-dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(file_path)

# source("C:/Users/JOE/Documents/R_utility_and_self_implementations/FAMD_plots_utility.R")
source("C:/Users/JOE/Documents/R_utility_and_self_implementations/colors_themes_utility.R")

################################################################################
# LOADING DATA AND PREPARING IT FOR THE AUTO-ENCODER
################################################################################

example_mixed_data_clustering_1=readRDS("../../open_data/example_mixed_data_clustering_1.rds")


for (k in 1:6) {
  example_mixed_data_clustering_1[,k]=as.numeric(as.character(example_mixed_data_clustering_1[,k]))
}

Cat1_dummied=predict(dummyVars(data=example_mixed_data_clustering_1, ~ Cat1),newdata=example_mixed_data_clustering_1[,7,drop=FALSE])
example_mixed_data_clustering_1_final=as.data.frame(cbind(example_mixed_data_clustering_1[,1:6],Cat1_dummied))


true_clusters=as.factor(c(rep(1,1000),rep(2,1000),rep(3,1000)))


example_mixed_data_clustering_1_final[,1:3]=apply(example_mixed_data_clustering_1_final[,1:3],2,function(x) {3*(x-min(x))/(max(x)-min(x))})
print(diag(var(example_mixed_data_clustering_1_final)))
example_mixed_data_clustering_1_final=as.matrix(example_mixed_data_clustering_1_final)


# example_mixed_data_clustering_1_final=scale(example_mixed_data_clustering_1_final)
################################################################################
# FAMD
################################################################################

FAMD_res_example_mixed_data_clustering_1=FAMD(example_mixed_data_clustering_1, ncp = ncol(example_mixed_data_clustering_1), graph = FALSE)


################################################################################
# Creating an auto-encoder
################################################################################


# Input = layer_input(shape=ncol(example_mixed_data_clustering_1_final))
# encoder_raw_1 = Input %>% layer_dense(units=8)
# encoder_activation_1 = encoder_raw_1 %>% layer_activation(activation="relu",name="encoder_activation1")
# encoder_raw_2 = encoder_activation_1 %>% layer_dense(units=6)
# encoder_activation_2 = encoder_raw_2 %>% layer_activation(activation="relu",name="encoder_activation2")
# encoder_raw_3 = encoder_activation_2 %>% layer_dense(units=4)
# encoder_activation_3 = encoder_raw_3 %>% layer_activation(activation="relu",name="encoder_activation3")
# decoder_raw = encoder_activation_3 %>% layer_dense(units=ncol(example_mixed_data_clustering_1_final))
# decoder_activation = decoder_raw %>% layer_activation(activation="sigmoid")


Input = layer_input(shape=ncol(example_mixed_data_clustering_1_final))
encoder_raw_1 = Input %>% layer_dense(units=4)
encoder_activation_1 = encoder_raw_1 %>% layer_activation(activation="relu",name="encoder_activation1")
decoder_raw = encoder_activation_1 %>% layer_dense(units=ncol(example_mixed_data_clustering_1_final))
decoder_activation = decoder_raw %>% layer_activation(activation="sigmoid")



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
  example_mixed_data_clustering_1_final, example_mixed_data_clustering_1_final,
  epochs = 200,
  validation_split = 0,
  verbose=0
)

###################
#Model evaluation on the training set
evaluation_score <- model %>% evaluate(example_mixed_data_clustering_1_final,
                                       example_mixed_data_clustering_1_final)

print(evaluation_score)


####################################################################################
# Looking at the hidden layer 
####################################################################################

layer_name <- "encoder_activation1"
intermediate_layer_model <- keras_model(inputs = model$input,
                                        outputs = get_layer(model, layer_name)$output)
intermediate_output <- predict(intermediate_layer_model, example_mixed_data_clustering_1_final)


kmeans_simple_autoencoder=kmeans(intermediate_output,center=3)


kmeans_autoencoder_classes_plot=make_FAMD_ind_plot_classes(FAMD_res_example_mixed_data_clustering_1,
                                                           classes=as.factor(kmeans_simple_autoencoder$cluster),
                                                    dims=c(1,2),custom_theme=theme_jh,color_scale=distinct_scale)
x11()
print(kmeans_autoencoder_classes_plot)



print(table(kmeans_simple_autoencoder$cluster,true_clusters))
