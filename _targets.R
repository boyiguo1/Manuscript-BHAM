## Load your packages, e.g. library(targets).
library(targets)
library(tarchetypes)
library(dotenv)

tar_option_set(
  packages = c("tidyverse", "dplyr", "rticles", "gtsummary", "here", "mgcv",
               "broom", "unglue", "knitr", "ggpubr", "xtable", "janitor", 
               "flextable", "BHAM", "BhGLM", "sparseGAM", "cosso", "glmnet"
               ),
  imports = c("BHAM", "BhGLM", "sparseGAM")
)

## Load your R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)




## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  #  Simulations Studies -----------------------------------------------------
  #* Main Study ####
  
  # tar_files(sim_main_path,
  #           list.files("./Simulation/main", recursive = T, full.names = T)),
  # 
  # success_rate = create_success_rate_table(sim_main_path),
  # 
  # tab_binom = make_sim_main_table(success_rate,
  #                                 dist = "binomial",
  #                                 measures = "auc"),
  # 
  # tab_gaus = make_sim_main_table(success_rate,
  #                                dist = "gaussian",
  #                                measures = "R2"),
  # 
  # tab_pois = make_sim_main_table(success_rate,
  #                                dist = "poisson",
  #                                measures = "mse"),
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
  tar_target(ECB_train_path,
             "Real_Data/Emory_Card_Biobank/Data/Analysis_data_first_cohort.csv",
             format = "file"),
  # tar_target(realdata_ECB_valid_path,
  #            "Real_Data/Emory_Card_Biobank/Data/Analysis_data_second_cohort.csv",
  #            format = "file"),
  tar_target(ECB_train_dat,
             readr::read_csv(ECB_train_path)),
  # tar_target(RD_ECB_valid_dat,
  #            readr::read_csv(realdata_ECB_valid_path)), 
  # 
  # #** GAM Screening ####
  tar_target(train_gam_screen,
             gam_screen(ECB_train_dat)),
  
  tar_target(ECB_cov,
             ECB_train_dat %>% 
               select(train_gam_screen %>% pull(var)) %>% 
               data.matrix),
  
  tar_target(ECB_outcome,
             ECB_train_dat %>% pull(death3yr)),
  
  # ** BHAM --------------------------------------------------------------- 
  mz_names = ECB_cov %>% colnames,
  sm_df = data.frame(
    Var = mz_names,
    Func = "s",
    Args ="bs='cr', k=5"
  ),
  
  sm_obj = construct_smooth_data(sm_df, ECB_train_dat),
  dsn_mat = sm_obj$data,
  bamlasso_raw = bamlasso(dsn_mat, ECB_outcome, family = "binomial",
                          group = make_group(names(dsn_mat))),
  
  cv_id = cv.bgam(bamlasso_raw)$foldid,
  
  bamlasso_tune = tune.bgam(bamlasso_raw, s0 = seq(0.005, 0.1, 0.01),
                            foldid = cv_id),
  
  # TODO: Use cv.bh to get the measures.
  
  # ** SB-GAM ---------------------------------------------------------------
  tar_target(bai_mdl_tune,
             cv.SBGAM( X = ECB_cov,
                       y = ECB_outcome,
                       family = "binomial", nfolds = 10,
                       nlambda = 10,
                       a = 1, b = 1)
  ),
  
  bai_cv_res = map_dfr(1:length(bai_mdl_tune$lambda0),
                       .f = function(i){
                         measure.glm(bai_mdl_tune$y.new, bai_mdl_tune$prevalid.mu[,i], family="binomial") %>% 
                           t %>% data.frame()
                       }) %>% 
    data.frame(lambda = bai_mdl_tune$lambda0,.),
  # measure.glm(bai_mdl_tune$y.new, bai_mdl_tune$prevalid.mu[,2], family="binomial"),
  
  # 
  # tar_target(bai_cv_pred,
  #   make_SB_cv(x  = ECB_cov, y = ECB_outcome, bai_mdl_tune, cv_id)
  # ),
  # 
  # tar_target(bai_cv_pred_32,
  #            make_SB_cv(x  = ECB_cov, y = ECB_outcome, list(lambda0.min = 32), cv_id)
  # ),
  
  # measure.glm(y = bai_cv_pred_32$y,
  #             y.fitted = bai_cv_pred_32$y.fitted,
  #             family = "binomial"),
  # 
  tar_target(op_bai_mdl_32,
             SBGAM(X = ECB_cov, y = ECB_outcome, family = "binomial", lambda0 = 32, a = 1, b = 1)),
  # op_bai_mdl = SBGAM(X = ECB_cov, y = ECB_outcome, family = "binomial", lambda0 = 2),
  # 
  # op_bai_msr_36 = measure.glm(y = ECB_outcome, y.fitted = op_bai_mdl_36$mu.pred, family = "binomial"),
  # op_bai_msr = measure.glm(y = ECB_outcome, y.fitted = op_bai_mdl$mu.pred, family = "binomial"),
  # tar_target(bai_cv_res,
  #   measure.glm(y = ECB_outcome, bai_cv_pred, family = "binomial")),
  # tar_target(bai_cv_res_36,
  #            measure.glm(y = ECB_outcome, op_bai_mdl_32$mu.pred, family = "binomial")),
  
  
  ## Split
  tar_target(split_id,
             sample(1:nrow(ECB_cov), 100, replace = F)),
  
  tmp_mdl = SBGAM(X=ECB_cov[-split_id, ], y = ECB_outcome[-split_id],
                  X.test = ECB_cov[split_id,], family = "binomial", lambda0 = 32,
                  a = 1, b = 1),
  
  tmp_msr = measure.glm(ECB_outcome[split_id], c(tmp_mdl$mu.pred), family = "binomial"),
  
  # ** COSSO ---------------------------------------------------------------
  # tar_target(cosso_tune,
  #            cosso(ECB_cov, ECB_outcome, family = "Binomial", nbasis=6)
  #            # %>%
  #              # tune.cosso()
  #              ),
  
  # tar_target(cosso_cv_pred,
  #            make_cosso_cv(cosso_tune, fold_id),
  # ),
  
  
  # #** Report ####
  # tar_render(real_data_report_ECB,
  #            "Real_Data/Emory_Card_Biobank/Report.Rmd",
  #            output_dir = "Real_Data/Emory_Card_Biobank/",
  #            output_file = "ECB_report.html"),
  
  
  #* Dataset 2 ---------------------------------------------------------------
  
  # ** Data Prep -------------------------------------------------------------
  
  tar_target(metabolomics_path,
             "./Real_Data/Adult_Obesity_Nontargeted_Metabolomics_Data.csv",
             format = "file"),

  mb_dat_raw = readr::read_csv(metabolomics_path),
  mb_dat = mb_dat_raw %>%
    clean_meta_dat(),

  tar_target(cov_path,
             "./Real_Data/Adult_Obesity_Nontargeted_Metabolomics_Sample_Metadata.csv",
             format = "file"),

  cov_dat_raw = readr::read_csv(cov_path),

  cov_dat = cov_dat_raw %>%
    clean_cov_dat(),


  ful_dat_raw = left_join(cov_dat, mb_dat),

  ful_dat = ful_dat_raw %>%
    filter(complete.cases(.)) %>%
    filter(out_HOMA_PC < 100,
           cov_Race != "O",
           Study %in% c("WLM", "STRRIDEPD")) %>%
    mutate(out_HOMA_std = scale(out_HOMA_PC) %>% as.numeric),
  train_dat = ful_dat %>% filter(Study == "WLM"),
  test_dat = ful_dat %>% filter(Study == "STRRIDEPD"),
  # 
  # ** Analysis --------------------------------------------------------------
  
  # ** Table 1 --------------------------------------------------------------
  
  
  # tbl_one = ful_dat %>% 
  #   tbl_summary(by = Study,
  #               include = starts_with("cov_")) %>% 
  #   add_overall(),
  
  # ** bgam --------------------------------------------------------------
  
  ## Linear Regression
  # base_mdl = lm(out_HOMA_std~cov_Age + cov_Sex + cov_Race + cov_Triglycerides_Baseline + cov_Weight_PC,
  #               data = train_dat),
  
  # mean(base_mdl$residuals^2)
  
  # data.frame(ful_dat$out_HOMA_PC, bgam_mdl$y, bgam_mdl$linear.predictors, base_mdl$fitted.values) %>% head()
  
  ## Lasso Model
  # cv_lasso_mdl = cv.glmnet(x = train_dat %>%
  #                            select(starts_with("mb")) %>%
  #                            data.matrix(),
  #           y = train_dat$out_HOMA_std,
  #           #penalty.factor = c(rep(0, 5), rep(1, 484))
  #           ),
  
  # lasso_mdl = glmnet(x = train_dat %>%
  #                      select(starts_with("mb")) %>%
  #                      data.matrix(),
  #                    y = train_dat$out_HOMA_std,
  #                    lambda = cv_lasso_mdl$lambda,
  #                    #penalty.factor = c(rep(0, 5), rep(1, 484))
  #                    ),
  
  
  ## Prepare dat
  bgam_dat = make_bgam_dat(train_dat, test_dat = test_dat),
  bgam_mdl = bamlasso(bgam_dat$train_dat, train_dat$out_HOMA_std, family = "gaussian",
                      ss=c(0.005, 0.05),
                      group = bgam_dat$group),

  tune_bgam_mdl = tune.bgam(bgam_mdl,s0 = seq(0.005, 0.1, 0.01)),
  
  # bglm_mdl_fnl = bamlasso(bgam_dat$train_dat, train_dat$out_HOMA_std, family = "gaussian",
  #                         ss=c(0.005, 0.05),
  #                         group = bgam_dat$group)
  
  ## bmlasso
  
  # bmlass_mdl = bmlasso(bgam_dat$train_dat, train_dat$out_HOMA_std, family = "gaussian",
  #                      ss=c(0.005, 0.05),
  #                      group = bgam_dat$group),
  
  
  
  # ,
  # mean((bgam_mdl$y - bgam_mdl$linear.predictors)^2),  
  
  
  # bgam_dml_tun = tune.bgam(bgam_mdl, nfolds = 5, s0 = seq(0.005, 0.1, 0.01)),
  
  # bgam_mdl = make_bgam_mdl(ful_dat),
  
  
  
  cv_bai_mdl = cv.SBGAM(y = ful_dat$out_HOMA_std, X = ful_dat %>% select(starts_with("mb_")) %>% as.matrix,  df=5, family = "gaussian", a = 1, b = 1,
                       max.iter=100, tol = 1e-6, nlambda0 = 10, nfolds = 10),
  bai_cv_res_2 = map_dfr(1:length(cv_bai_mdl$lambda0),
                       .f = function(i){
                         measure.glm(cv_bai_mdl$y.new, cv_bai_mdl$prevalid.mu[,i], family = "gaussian") %>% 
                           t %>% data.frame()
                       }) %>% 
    data.frame(lambda = cv_bai_mdl$lambda0,.),
  
  bai_mdl_fnl = SBGAM(y = ful_dat$out_HOMA_std, X = ful_dat %>% select(starts_with("mb_")) %>% as.matrix,  df=5, family = "gaussian", a = 1, b = 1,
                       max.iter=100, tol = 1e-6, lambda0 = 58),
  
  # bai_mdl = SBGAM(ful_dat$out_HOMA_PC, X = ful_dat %>% select(starts_with("mb_")) %>% as.matrix, df=5, family = "gaussian" ,
  #                lambda0 = cv_bai_mdl$lambda0.min, a = 1, b = 1,
  #                max.iter=100, tol = 1e-6, print.iter=FALSE),
  # 
  # cv_bai_mdl_final = cv.SBGAM(y = ful_dat$out_HOMA_PC, X = ful_dat %>% select(starts_with("mb_")) %>% as.matrix, 
  #                             df=5, family = "gaussian", a = 1, b = 1,
  #                             lambda0 = cv_bai_mdl$lambda0.min,
  #                     max.iter=100, tol = 1e-6),
  
  
  # bai_train_msr <- make_null_res(fam_fun$family)
  # bai_test_msr <- measure.glm(test_dat$y, bai_mdl$mu.pred, family = fam_fun$family) 
  
  
  
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

  # tar_target(fig_path,
  #            "Manuscript/Fig/",
  #            format = "file"),
  #
  #* Assemble Manuscript ####
  tar_render(manu, "Manuscript/00-main.Rmd",
             output_file = "SS_GAM.pdf")
  
)

