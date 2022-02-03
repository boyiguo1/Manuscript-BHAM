## Load your packages, e.g. library(targets).
library(targets)
library(tarchetypes)
library(dotenv)

tar_option_set(
  packages = c("tidyverse", "dplyr", "rticles", "gtsummary", "mgcv", #"here", 
               "broom", "unglue", "knitr", "ggpubr", "xtable", "janitor", 
               "flextable", "BHAM", "BhGLM", "sparseGAM",  "glmnet", "yardstick",
               "rmarkdown"#, "cosso",
  ),
  imports = c( "BHAM", "BhGLM", "sparseGAM")
)

## Load your R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)




## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  
  #  Simulations Studies -----------------------------------------------------
  #* Main Simulation Study ####
  tar_files(sim_main_path,
            list.files("/data/user/boyiguo1/bgam/sim_res/main", recursive = T, full.names = T)),

  sim_success_rate = create_success_rate_table(sim_main_path),
  
  sim_mdl_fail = create_mdl_fail_rate(sim_success_rate),

  sim_binom_tab = make_sim_main_table(sim_success_rate,
                                  dist = "binomial",
                                  measures = "auc"),
  
  sim_binom_var_select_raw = sim_var_select_raw(sim_success_rate,
                                                       dist = "binomial"),
  # plot_var_select(sim_binom_var_select_raw[[1]])
  sim_binom_comp_select_raw = sim_comp_select_raw(sim_success_rate,
                                                dist = "binomial"),

  # plot_comp_select(sim_binom_comp_select_raw[[4]])
  
  # Visual Presentation of Var Selection
  # make_sim_var_metric_raw(sim_binom_var_select_raw[[4]]) %>% 
  # ggplot() + 
  #   geom_boxplot(aes(x = Method, y = value)) +
  #   theme(axis.text.x = element_text( angle = 90))+
  #   facet_wrap(~Metric, ncol = 3, nrow = 1, scales = "free"),

  # Table Presentation of Var Selection
  # make_sim_var_metric_raw(sim_binom_var_select_raw[[4]]) %>% 
  #   group_by(Method, Metric) %>% 
  #   summarise(mean = mean(value, na.rm = TRUE)#, sd = sd(value, na.rm = TRUE)
  #             ) %>% 
  #   pivot_wider(names_from = Method, values_from = mean),
  
  
  sim_gaus_tab = make_sim_main_table(sim_success_rate,
                                 dist = "gaussian",
                                 measures = "R2"),
  
  sim_gaus_var_select_raw = sim_var_select_raw(sim_success_rate,
                                                dist = "gaussian"),
  # plot_var_select(sim_gaus_var_select_raw[[3]])
  sim_gaus_comp_select_raw = sim_comp_select_raw(sim_success_rate,
                                                  dist = "gaussian"),
  # sim_gaus_var_select = make_sim_var_select_table(sim_success_rate,
  #                                                  dist = "gaussian"),
  
  sim_tim_tab = make_time_table(sim_success_rate),
  
  
  #  Real Data Analysis -----------------------------------------------------
  # * Emory Card Biobank ####
  # ** Load Data ####
  tar_target(ECB_train_path,
             "Real_Data/Emory_Card_Biobank/Data/Analysis_data_first_cohort.csv",
             format = "file"),
  
  tar_target(ECB_train_dat,
             readr::read_csv(ECB_train_path)),
  
  # ** GAM Screening ####
  tar_target(ECB_gam_screen,
             gam_screen(ECB_train_dat)),
  
  tar_target(ECB_cov,
             ECB_train_dat %>% 
               select(ECB_gam_screen %>% pull(var)) %>% 
               data.matrix),
  
  tar_target(ECB_outcome,
             ECB_train_dat %>% pull(death3yr)),
  
  # ** BHAM --------------------------------------------------------------- 
  ECB_mz_names = ECB_cov %>% colnames,
  ECB_sm_df = data.frame(
    Var = ECB_mz_names,
    Func = "s",
    Args ="bs='cr', k=5"
  ),
  
  ECB_sm_obj = construct_smooth_data(ECB_sm_df, ECB_train_dat),
  ECB_dsn_mat = ECB_sm_obj$data,
  
  ECB_bamlasso_raw = bamlasso(ECB_dsn_mat, ECB_outcome, family = "binomial",
                          group = make_group(names(ECB_dsn_mat))),
  
  ECB_bamlasso_cv = tune.bgam(ECB_bamlasso_raw, s0 = seq(0.005, 0.1, 0.01)),
  
  ECB_bamlasso_fnl = bamlasso(ECB_dsn_mat, ECB_outcome, family = "binomial",
                          group = make_group(names(ECB_dsn_mat)),
                          ss = c(0.095, 0.5)),
  
  ECB_bamlasso_insample_msr = measure.bh(ECB_bamlasso_fnl),
  ECB_bamlasso_var = bamlasso_var_selection(ECB_bamlasso_fnl),
  # TODO: Use cv.bh to get the measures.
  
  # ** SB-GAM ---------------------------------------------------------------
  ECB_SBGAM_cv_raw = cv.SBGAM( X = ECB_cov,
                       y = ECB_outcome,
                       family = "binomial", nfolds = 10,
                       nlambda = 10,
                       a = 1, b = 1),
  
  ECB_SBGAM_cv = summary_cv_SBGAM(ECB_SBGAM_cv_raw, family = "binomial"),
  # ECB_SBGAM_cv_per_fold = summary_cv_SBGAM_per_fold(ECB_SBGAM_cv_raw, family = "binomial"),
  # ggplot(ECB_SBGAM_cv_per_fold)+
  #   geom_violin(aes(x = lambda0, y = deviance)),
  
  ECB_SBGAM_fnl = SBGAM(X = ECB_cov, y = ECB_outcome, family = "binomial", lambda0 = 24, a = 1, b = 1),

    tar_target(ECB_SBGAM_insample_msr,
             measure.glm(y = ECB_outcome, ECB_SBGAM_fnl$mu.pred, family = "binomial")),
  
  ECB_SBGAM_var = (ECB_cov %>% colnames())[ECB_SBGAM_fnl$classifications!=0],
  
  
  # 
  # train_msr = data.frame("method" = c("BHAM-SSL", "SB-GAM"),
  #                        rbind(ECB_bamlasso_insample_msr ,ECB_SBGAM_insample_msr)),

  
  #* Weight Loss Maintainace Study---------------------------------------------------------------
  
  # ** Data Prep -------------------------------------------------------------
  tar_target(WLM_metabolomics_path,
             "./Real_Data/Adult_Obesity_Nontargeted_Metabolomics_Data.csv",
             format = "file"),
  
  WLM_mb_dat_raw = readr::read_csv(WLM_metabolomics_path),
  WLM_mb_dat = WLM_mb_dat_raw %>%
    clean_meta_dat(),
  
  tar_target(WLM_cov_path,
             "./Real_Data/Adult_Obesity_Nontargeted_Metabolomics_Sample_Metadata.csv",
             format = "file"),
  
  WLM_cov_dat_raw = readr::read_csv(WLM_cov_path),
  
  WLM_cov_dat = WLM_cov_dat_raw %>%
    clean_cov_dat(),
  
  WLM_ful_dat_raw = left_join(WLM_cov_dat, WLM_mb_dat),
  
  WLM_ful_dat = WLM_ful_dat_raw %>%
    filter(complete.cases(.)) %>%
    filter(out_HOMA_PC < 100,
           cov_Race != "O",
           Study %in% c("WLM", "STRRIDEPD")) %>%
    mutate(out_HOMA_std = scale(out_HOMA_PC) %>% as.numeric),
  WLM_train_dat = WLM_ful_dat %>% filter(Study == "WLM"),
  WLM_test_dat = WLM_ful_dat %>% filter(Study == "STRRIDEPD"),
  
  # ** Table 1 --------------------------------------------------------------
  # WLM_tbl_one = WLM_ful_dat %>% 
  #   tbl_summary(by = Study,
  #               include = starts_with("cov_")) %>% 
  #   add_overall(),
  
  # ** BHAM-CD --------------------------------------------------------------
  
  ## Prepare dat
  WLM_bgam_dat = make_bgam_dat(WLM_train_dat, test_dat = WLM_test_dat),
  WLM_bamlasso_raw = bamlasso(WLM_bgam_dat$train_dat, WLM_train_dat$out_HOMA_std, family = "gaussian",
                              ss=c(0.005, 0.5),
                              group = WLM_bgam_dat$group),
  
  WLM_bamlasso_cv = tune.bgam(WLM_bamlasso_raw, s0 = seq(0.005, 0.1, 0.01)),

  
  WLM_bamlasso_fnl = bamlasso(WLM_bgam_dat$train_dat, WLM_train_dat$out_HOMA_std, family = "gaussian",
                          ss=c(0.025, 0.5),
                          group = WLM_bgam_dat$group),
  # measure.bh(bglm_mdl_fnl),
  WLM_bamlasso_var = bamlasso_var_selection(WLM_bamlasso_fnl),
  
  # # ** BHAM-IWLS --------------------------------------------------------------
  # WLM_bgam_raw = bgam(out_HOMA_std~.-out_HOMA_std, 
  #                     data = data.frame(out_HOMA_std =  WLM_train_dat$out_HOMA_std, WLM_bgam_dat$train_dat),
  #                     family = "gaussian",
  #                             prior = mde(),
  #                             group = WLM_bgam_dat$group),
  # 
  # WLM_bgam_cv = tune.bgam(WLM_bgam_raw,s0 = seq(0.005, 0.1, 0.01)),
  
  # ** SB_GAM ---------------------------------------------------------------
  
  WLM_SBGAM_cv_raw = cv.SBGAM(
    y = WLM_train_dat$out_HOMA_std,
    X = WLM_train_dat %>% select(starts_with("mb_")) %>% as.matrix,
    df=5, family = "gaussian", a = 1, b = 1,
    max.iter=100, tol = 1e-6, nlambda0 = 10, nfolds = 10),
  
  WLM_SBGAM_cv = summary_cv_SBGAM(WLM_SBGAM_cv_raw),
  # WLM_SBGAM_cv_per_fold = summary_cv_SBGAM_per_fold(WLM_SBGAM_cv_raw),
  # 
  # ggplot(WLM_SBGAM_cv_per_fold %>% filter(lambda0!=23.7))+
  #   geom_violin(aes(x = lambda0, y = deviance)),
  
  WLM_SBGAM_fnl = SBGAM(
    y = WLM_train_dat$out_HOMA_std,
    X = WLM_train_dat %>% select(starts_with("mb_")) %>% as.matrix, 
    df=5, family = "gaussian", a = 1, b = 1,
    max.iter=100, tol = 1e-6, lambda0 = 58),
  WLM_SBGAM_vars = (WLM_train_dat %>% select(starts_with("mb_")) %>% as.matrix %>% colnames())[WLM_SBGAM_fnl$classifications!=0],
  
  # measure.glm(WLM_train_dat$out_HOMA_std, WLM_SBGAM_fnl$mu.pred, family = "gaussian"),  
  
  # Manuscript --------------------------------------------------------------
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
  
  tar_target(tbl_real_time,
             create_real_data_time_tbl(  # Introduce dependency
               ECB_bamlasso_cv,
               ECB_bamlasso_fnl,
               ECB_SBGAM_cv_raw,
               ECB_SBGAM_fnl,
               WLM_bamlasso_cv,
               WLM_bamlasso_fnl,
               WLM_SBGAM_cv_raw,
               WLM_SBGAM_fnl)),
  
  #* Assemble Manuscript ####
  tar_render(manu, "Manuscript/00-main.Rmd",
             output_file = "SS_GAM.pdf"),
  
  # tar_render(manu_ENAR, "Manuscript/00-main_ENAR.Rmd",
  #            output_file = "SS_GAM-ENAR.pdf"),
  
)

