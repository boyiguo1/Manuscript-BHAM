## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  #  Simulations Studies -----------------------------------------------------
  #* Main Study ####
  
  tar_files(sim_main_path,
            list.files("./Simulation/main", recursive = T, full.names = T)),
  
  success_rate = create_success_rate_table(sim_main_path),
  
  tab_binom = make_sim_main_table(success_rate,
                                  dist = "binomial",
                                  measures = "auc"),
  
  tab_gaus = make_sim_main_table(success_rate,
                                 dist = "gaussian",
                                 measures = "R2"),
  
  tab_pois = make_sim_main_table(success_rate,
                                 dist = "poisson",
                                 measures = "mse"),
  
  
  
  
  # 
  # tar_target(binom_plot,
  #            make_sim_main_plots(
  #              success_rate,
  #              "binomial",
  #              list("test.deviance", "test.auc", "test.misclassification"))
  #            ),
  # 
  # tar_target(gaussian_plot,
  #            make_sim_main_plots(
  #              success_rate,
  #              "gaussian",
  #              list("test.R2", "test.mse", "test.mae"))),
  # 
  # tar_target(poisson_plot,
  #            make_sim_main_plots(
  #              success_rate,
  #              "poisson",
  #              list("test.deviance", "test.mse", "test.mae"))
  #            ),
  # 
  # #* Tuning Study ####
  # tar_render(sim_tuning_report,
  #            "Simulation/tuning/tuning_study_report.Rmd",
  #            output_dir = "Simulation/tuning/",
  #            output_file = "sim_tuning_report.html"),
  
  
  #  Real Data Analysis -----------------------------------------------------
  # #* Emory Card Biobank ####
  # #** Load Data ####
  # tar_target(realdata_ECB_train_path,
  #            "Real_Data/Emory_Card_Biobank/Data/Analysis_data_first_cohort.csv",
  #            format = "file"),
  # tar_target(realdata_ECB_valid_path,
  #            "Real_Data/Emory_Card_Biobank/Data/Analysis_data_second_cohort.csv",
  #            format = "file"),
  # tar_target(RD_ECB_train_dat,
  #            readr::read_csv(realdata_ECB_train_path)),
  # tar_target(RD_ECB_valid_dat,
  #            readr::read_csv(realdata_ECB_valid_path)), 
  # 
  # #** GAM Screening ####
  # tar_target(train_gam_screen,
  #            gam_screen(RD_ECB_train_dat)),
  # 
  # #** Report ####
  # tar_render(real_data_report_ECB,
  #            "Real_Data/Emory_Card_Biobank/Report.Rmd",
  #            output_dir = "Real_Data/Emory_Card_Biobank/",
  #            output_file = "ECB_report.html"),
  
  
  # Dataset 2 ---------------------------------------------------------------
  
  # * Data Prep -------------------------------------------------------------
  
  tar_target(metabolomics_path,
             "./Real_Data/Adult_Obesity_Nontargeted_Metabolomics_Data.csv",
             format = "file"),
  
  mb_dat_raw = readr::read_csv(metabolomics_path),
  mb_dat = mb_dat_raw %>% 
    clean_meta_dat(),
  
  tar_target(cov_path,
             "./Real_Data/Copy of Adult_Obesity_Nontargeted_Metabolomics_Sample_Metadata.csv",
             format = "file"),
  
  cov_dat_raw = readr::read_csv(cov_path),
  
  cov_dat = cov_dat_raw %>% 
    clean_cov_dat(),
  
  
  ful_dat_raw = left_join(cov_dat, mb_dat),
  
  ful_dat = ful_dat_raw %>% 
    filter(complete.cases(.)),
  
  # * Analysis --------------------------------------------------------------

# ** Table 1 --------------------------------------------------------------

  
  tbl_one = ful_dat %>% 
    tbl_summary(by = Study,
                include = starts_with("cov_")) %>% 
    add_overall(),
  
# ** bgam --------------------------------------------------------------

  bgam_mdl = make_bgam_mdl(ful_dat),
  
  bgam_dml_tun = tune.bgam(bgam_mdl, nfolds = 5, s0 = seq(0.005, 0.1, 0.01)),
  
  cv_bai_mdl = cv.SBGAM(y = ful_dat$out_HOMA_PC, X = ful_dat %>% select(starts_with("mb_")) %>% as.matrix,  df=5, family = "gaussian", a = 1, b = 1,
                       max.iter=100, tol = 1e-6),


  bai_mdl = SBGAM(ful_dat$out_HOMA_PC, X = ful_dat %>% select(starts_with("mb_")) %>% as.matrix, df=5, family = "gaussian" ,
                 lambda0 = cv_bai_mdl$lambda0.min, a = 1, b = 1,
                 max.iter=100, tol = 1e-6, print.iter=FALSE),


# bai_train_msr <- make_null_res(fam_fun$family)
# bai_test_msr <- measure.glm(test_dat$y, bai_mdl$mu.pred, family = fam_fun$family) 



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
  
  # tar_target(fig_path,
  #            "Manuscript/Fig/",
  #            format = "file"),
  # 
  #* Assemble Manuscript ####
  tar_render(manu, "Manuscript/00-main.Rmd", 
             # params = list(
             #   intro = intro_path,
             #   method = method_path,
             #   sim = sim_path,
             #   RD = real_data_path,
             #   disc = disc_path),
             output_file = "SS_GAM.pdf")
  
)
