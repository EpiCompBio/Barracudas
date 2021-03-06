file_find_replace <- function(filepath, pattern, replacement) {
  file_contents <- readLines(filepath)
  updated_contents <- gsub(x = file_contents, pattern = pattern, replacement = replacement)
  cat(updated_contents, file = filepath, sep = "\n")
}


my_dir <- "C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas/code/scripts_joel_HPC_V4_males"
setwd(my_dir)


# Apply the function to each of the R scripts in the directory
my_r_scripts <- list.files(path = my_dir, pattern = "(r|R)$")

for (r_script in my_r_scripts) {
  file_find_replace(r_script,
                    "female",
                    "male")
}


for (r_script in my_r_scripts ) {
  file_find_replace(r_script,
                    "results_joel_HPC_V5",
                    "results_joel_HPC_V4")
}


for (r_script in my_r_scripts ) {
  file_find_replace(r_script,
                    "var_groupings_V5",
                    "var_groupings_V4")
}

for (r_script in my_r_scripts ) {
  file_find_replace(r_script,
                    "HW_mod_controls",
                    "HW_mod_no_obesity")
}



