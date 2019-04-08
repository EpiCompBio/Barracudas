setwd("/Users/abieast/Documents/Imperial/Translational_Data_Science/Project")

#### Ordered Factors ####
multi_morbid <- readRDS("Data/kamila_multi_morbid_ordinal_keep.rds")
multi_morbid_female <- multi_morbid[multi_morbid$Sex == "Female",]
multi_morbid_male <- multi_morbid[multi_morbid$Sex == "Male", ]
saveRDS(multi_morbid_female, "Data/multi_morbid_female.rds")
saveRDS(multi_morbid_male, "Data/multi_morbid_male.rds")

#### Ordinal Continuous ####
multi_morbid_cont <- readRDS("Data/kamila_multi_morbid_ordinal_continuous.rds")
multi_morbid_female_continuous <- multi_morbid_cont[multi_morbid_cont$Sex == "Female", ]
multi_morbid_male_continuous <- multi_morbid_cont[multi_morbid_cont$Sex == "Male", ]
saveRDS(multi_morbid_female_continuous, "Data/multi_morbid_female_continuous.rds")
saveRDS(multi_morbid_male_continuous, "Data/multi_morbid_male_continuous.rds")
