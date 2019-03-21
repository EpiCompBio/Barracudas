############################################################################################################
# Groupings for the full dataset
############################################################################################################

Disease = c("mi","angina","stroke","htn","obese","dvt_asthma_copd_atopy","diabetes","no_chronic")
Demographics = c("birth_year","birth_month",'Sex',"Townsend_depr_index",
                 "white_british_irish","mixed","Asian","Black_african","Chinese", "other_ethnic")
BMI_related = c("waist_circum","hip_circum","Height","height_sitting","sitting_height","BMI",
                "seated_box_height","Weight","body_fat_perc","whole_body_fat_mass","whole_body_water_mass")
Activity = c("days_walked_over10min","Duration_walks","n_days_moderate_ex","Duration_moderate_ex",
             "n_days_vigorous_ex","Duration_vigorous_ex","freq_climb_stairs_4wks",
             "freq_walked_for_pleasure_4wks", "Duration_pleasure_walks","time_tv",
             "time_computer","time_driving")
Vital_signs = c("heart_rate","diastolic_bp","SBP_automated")
Tobacco = c("smokers_in_house","tob_expo_home","tob_expo_outside","smoker","light_smoker",
            "ever_smoker","current_smoker")
Alcohol = c("Alc_intake_freq","current_etoh","ever_etoh")
Dietary = c("veg_intake_cooked","veg_intake_raw","fresh_fruit","dried_fruit",
            "oily_fish_intake","non_oily_fish_intake","processed_meat","poultry","beef_intake",
            "lamb_intake","pork_intake","cheese_intake","bread_intake","cereal_intake",
            "salt_added_food","tea_intake","coffee_intake","water_intake","varition_in_diet",
            "major_diet_change_5yr","spread_use")   
Med_surg_hx = c("self_reported_surgery","previous_surgery","pacemaker")

############################################################################################################
# Groupings with the HW PCs
############################################################################################################




Disease = c("mi","angina","stroke","htn","obese","dvt_asthma_copd_atopy","diabetes","no_chronic")
Demographics = c("birth_year","birth_month",'Sex',"Townsend_depr_index",
                 "white_british_irish","mixed","Asian","Black_african","Chinese", "other_ethnic")
BMI_related = c("HW_PCA_PC1","HW_PCA_PC2")
Activity = c("days_walked_over10min","Duration_walks","n_days_moderate_ex","Duration_moderate_ex",
             "n_days_vigorous_ex","Duration_vigorous_ex","freq_climb_stairs_4wks",
             "freq_walked_for_pleasure_4wks", "Duration_pleasure_walks","time_tv",
             "time_computer","time_driving")
Vital_signs = c("heart_rate","diastolic_bp","SBP_automated")
Tobacco = c("smokers_in_house","tob_expo_home","tob_expo_outside","smoker","light_smoker",
            "ever_smoker","current_smoker")
Alcohol = c("Alc_intake_freq","current_etoh","ever_etoh")
Dietary = c("veg_intake_cooked","veg_intake_raw","fresh_fruit","dried_fruit",
            "oily_fish_intake","non_oily_fish_intake","processed_meat","poultry","beef_intake",
            "lamb_intake","pork_intake","cheese_intake","bread_intake","cereal_intake",
            "salt_added_food","tea_intake","coffee_intake","water_intake","varition_in_diet",
            "major_diet_change_5yr","spread_use")   
Med_surg_hx = c("self_reported_surgery","previous_surgery","pacemaker")