#This is R script to run a markdown file in order to keep objects in the environment for intermediate modifications
setwd("C:/ali_working/fire_fuel/RAVG_revision/Archive/GitHub")
rmarkdown::render("SW_RAVG_index_and_sensor_eval.Rmd", clean=FALSE)
#note the output does not automatically open - it is stored in wd


