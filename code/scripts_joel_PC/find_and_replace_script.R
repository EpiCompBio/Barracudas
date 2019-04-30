file_find_replace <- function(filepath, pattern, replacement) {
  file_contents <- readLines(filepath)
  updated_contents <- gsub(x = file_contents, pattern = pattern, replacement = replacement)
  cat(updated_contents, file = filepath, sep = "\n")
}


my_dir <- "C:/Users/JOE/Documents/Imperial College 2018-2019/Translational Data Science/Barracudas/code/scripts_joel_HPC_V5"
setwd(my_dir)


# Apply the function to each of the R scripts in the directory
my_r_scripts <- list.files(path = my_dir, pattern = "(r|R)$")

for (r_script in my_r_scripts ) {
  file_find_replace(r_script,
                    "processed_V2",
                    "processed_V5")
}


for (r_script in my_r_scripts ) {
  file_find_replace(r_script,
                    "results_joel_HPC_V2",
                    "results_joel_HPC_V5")
}


for (r_script in my_r_scripts ) {
  file_find_replace(r_script,
                    "var_groupings_V2",
                    "var_groupings_V5")
}

for (r_script in my_r_scripts ) {
  file_find_replace(r_script,
                    "HW_mod",
                    "HW_mod_controls")
}