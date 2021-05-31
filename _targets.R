library(targets)
library(tarchetypes)

# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

options(tidyverse.quiet = TRUE)
# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr", "tidyverse", "rticles", "gtsummary", "here"))

# End this file with a list of target objects.
list(
  # tar_target(data, data.frame(x = sample.int(100), y = sample.int(100))),
  
  
  #  Real Data Analysis -----------------------------------------------------
  #* Emory Card Biobank ####
  tar_target(realdata_ECB_train_path,
             "Real_Data/Emory_Card_Biobank/Data/Analysis_data_first_cohort.csv",
             format = "file"),
  tar_target(realdata_ECB_valid_path,
             "Real_Data/Emory_Card_Biobank/Data/Analysis_data_second_cohort.csv",
             format = "file"),
  tar_target(RD_ECB_train_dat,
             readr::read_csv(realdata_ECB_train_path)),
  tar_target(RD_ECB_valid_dat,
             readr::read_csv(realdata_ECB_valid_path)), 
  tar_render(real_data_report_ECB,
             "Real_Data/Emory_Card_Biobank/Report.Rmd"),
  
  
  # Manuscript --------------------------------------------------------------
  #* Assemble Manuscript ####
  tar_render(manu, "Manuscript/main.Rmd", output_file = "SS_GAM.pdf")
  
)
