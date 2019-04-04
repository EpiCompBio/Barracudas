setwd("C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas")


FAMD_multi_morbid_res=readRDS("../real_data_from_HPC/processed_V2/FAMD_ordinal_factors_multi_morbid_female_res.rds")


FAMD_multi_morbid_res=readRDS("../real_data_from_HPC/processed_V3/multi_morbid_male_ordinal_continuous_HW_PCA.rds")




multi_morbid_res=readRDS("../real_data_from_HPC/processed_V3/multi_morbid_male_ordinal_continuous_HW_PCA.rds")

multi_morbid_res=readRDS("../real_data_from_HPC/processed_V3/multi_morbid_female_ordinal_continuous_HW_PCA.rds")

##############
FAMD_multi_morbid_res=readRDS("../real_data_from_HPC/processed_V3/FAMD_ordinal_continuous_multi_morbid_male_res.rds")
print(FAMD_multi_morbid_res$eig)


FAMD_multi_morbid_res=readRDS("../real_data_from_HPC/processed_V3/FAMD_ordinal_continuous_multi_morbid_female_res.rds")
print(FAMD_multi_morbid_res$eig)





##############
FAMD_multi_morbid_res=readRDS("../real_data_from_HPC/processed_V3/FAMD_ordinal_factors_multi_morbid_male_res.rds")
print(FAMD_multi_morbid_res$eig)


FAMD_multi_morbid_res=readRDS("../real_data_from_HPC/processed_V3/FAMD_ordinal_factors_multi_morbid_female_res.rds")
print(FAMD_multi_morbid_res$eig)

