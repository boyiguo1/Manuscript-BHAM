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

  #  Simulations Studies -----------------------------------------------------
  #* Tuning Study ####
  tar_render(sim_tuning_report,
             "Simulation/tuning/tuning_study_report.Rmd",
             output_dir = "Simulation/tuning/",
             output_file = "sim_tuning_report.html"),
    
  
  #  Real Data Analysis -----------------------------------------------------
  #* Emory Card Biobank ####
  #** Load Data ####
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
  #** Report ####
  # tar_render(real_data_report_ECB,
  #            "Real_Data/Emory_Card_Biobank/Report.Rmd",
  #            output_dir = "Real_Data/Emory_Card_Biobank/",
  #            output_file = "ECB_report.html"),
  # 
  
  # Manuscript --------------------------------------------------------------

  #* Section Paths ####
  tar_target(intro_path,
             "Manuscript/01-Intro.Rmd",
             format = "file"),
  
  tar_target(method_path,
             "Manuscript/02-Method.Rmd",
             format = "file"),
  
  tar_target(sim_path,
             "Manuscript/03-Simulation.Rmd",
             format = "file"),
  
  tar_target(real_data_path,
             "Manuscript/04-Real_Data.Rmd",
             format = "file"),
  
  tar_target(disc_path,
             "Manuscript/05-Discussion.Rmd",
             format = "file"),
  tar_target(fig_path,
             "Manuscript/Fig/",
             format = "file"),
  
  #* Assemble Manuscript ####
  tar_render(manu, "Manuscript/main.Rmd", 
             # params = list(
             #   intro = intro_path,
             #   method = method_path,
             #   sim = sim_path,
             #   RD = real_data_path,
             #   disc = disc_path),
             output_file = "SS_GAM.pdf")
  
)
