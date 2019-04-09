setwd("/Users/abieast/Documents/Imperial/Translational_Data_Science/Project")

##### Scaled Data #####

multi_morbid <- readRDS("Data/KAMILA_V2/multi_morbid_ordinal_continuous.rds")

# Remove outcome columns
multi_morbid<- multi_morbid[ ,-c(1:9)]

# Reorder columns
multi_morbid <- multi_morbid[ , c(1, 2, 4, 5, 6, 7, 8, 9, 10,11,12,13,14,15, 16, 17, 18, 19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,
                                                    34,35,36,37,38,39,40,41,42,43,44,45,46,47,64, 3, 48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,
                                                    63,65)]

saveRDS(multi_morbid,"Data/KAMILA_V2/kamila_scaled_multi_morbid_continuous.rds")

##### Unscaled Data #####

multi_morbid_unscaled <- readRDS("Data/KAMILA_V2/Unscaled/unscaled_multi_morbid_ordinal_continuous.rds")

# Remove outcome columns
multi_morbid_unscaled<- multi_morbid_unscaled[ ,-c(1:9)]

# Reorder columns

multi_morbid_unscaled <- multi_morbid_unscaled[ , c(1, 2, 4, 5, 6, 7, 8, 9, 10,11,12,13,14,15, 16, 17, 18, 19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,
                                  34,35,36,37,38,39,40,41,42,43,44,45,46,47,64, 3, 48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,
                                  63,65)]

saveRDS(multi_morbid,"Data/KAMILA_V2/unscaled_kamila_multi_morbid_continuous.rds")
