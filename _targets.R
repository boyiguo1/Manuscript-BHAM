## Load your packages, e.g. library(targets).
library(targets)
library(tarchetypes)
# library(dotenv)

tar_option_set(
  packages = c("tidyverse", "dplyr", "rticles", #"gtsummary", "gt", 
               "broom", "unglue", "knitr", "ggpubr", "xtable", "janitor", 
               "flextable", 
               "mgcv", "BHAM", "BhGLM", "sparseGAM",  "glmnet", "yardstick",
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
            list.files("/data/user/boyiguo1/bgam/sim_res_20220220/main/", recursive = T, full.names = T)),
  sim_success_rate = create_success_rate_table(sim_main_path),
  sim_mdl_fail = create_mdl_fail_rate(sim_success_rate),
  sim_pred_raw = create_raw_data(sim_success_rate),
  sim_time_raw = create_raw_data(sim_success_rate, section = "time"),
  
  #** Binomial Outcome  ####
  #*** Prediction  ####
  sim_binom_tab = make_sim_main_table(sim_success_rate,
                                      dist = "binomial",
                                      measures = "auc"),
  
  sim_binom_tab_latex = format_predict_tbls(
    sim_binom_tab,
    caption = "The average and standard deviation of the out-of-sample area under the curve measures
    for binomial outcomes over 50 iterations. The models of comparison include the proposed Bayesian
    hierarchical additive model (BHAM), linear LASSO model (LASSO), component selection and smoothing
    operator (COSSO), adaptive COSSO, mgcv, sparse Bayesian generalized additive model (SB-GAM), and
    spikeSlabGAM model. mgcv doesn't provide
    estimation when number of parameters exceeds sample size i.e. p = 100, 200.",
    label = "tab:bin_auc")%>% 
    cat(file = "Manuscript/Tabs/sim_binom_tab.tex"),
  
  # Percentage of change
  sim_binom_pred_bham_lasso = sim_pred_raw %>% filter(dist == "binomial") %>%
    compare_methods(method_1 = "bamlasso", method_2 = "lasso", measures="auc") %>% 
    with(sprintf("%.0f%% (%.0f%%)", median*100, IQR*100)),
  
  #*** Bi-level Selection  ####
  
  
  sim_binom_comp_select_raw = sim_comp_select_raw(sim_success_rate,
                                                  dist = "binomial"),
  # plot_comp_select(sim_binom_comp_select_raw[[1]])
  
  #*** Variable Selection  ####
  sim_binom_var_select_raw = sim_var_select_raw(sim_success_rate,
                                                dist = "binomial"),
  
  # plot_var_select(sim_binom_var_select_raw[[1]])  
  
  
  # Table Presentation of Var Selection
  tmp = make_sim_var_metric_raw(),
  sim_binom_var_select_tbl = map_dfr(1:5,
                                     ~make_sim_var_metric_raw(sim_binom_var_select_raw[[.x]],
                                                              sim_binom_comp_select_raw[[.x]])) %>%
    group_by(p, Method, Metric) %>%
    summarise(mean = mean(value, na.rm = TRUE)#, sd = sd(value, na.rm = TRUE)
    ) %>%
    pivot_wider(names_from = Method, values_from = mean) %>% 
    ungroup(),
  
  sim_binom_var_select_tbl_latex = sim_binom_var_select_tbl %>% 
    format_var_slct_tbls(caption = "The variable selection performance of binomial simulations,
                         measured by positive predictive value (precision), true positive rate (recall),
                         and Matthews correlation coefficient (MCC), for the high-dimensional methods 
                         averaged over 50 iterations. The models of comparison include the proposed Bayesian
                         hierarchical additive model (BHAM), linear LASSO model (LASSO), component selection and
                         smoothing operator (COSSO), adaptive COSSO, sparse Bayesian generalized additive model (SB-GAM),
                         and spikeSlabGAM model. MCC is ill-defined when $p=4$ simulation (no true negative), and hence omitted for all methods.",
                         label = "tab:sim_binom_var_select")%>% 
    cat(file = "Manuscript/Tabs/sim_binom_var_slct_tab.tex"),
  # Visual Presentation of Var Selection
  # make_sim_var_metric_raw(sim_binom_var_select_raw[[4]]) %>% 
  # ggplot() + 
  #   geom_boxplot(aes(x = Method, y = value)) +
  #   theme(axis.text.x = element_text( angle = 90))+
  #   facet_wrap(~Metric, ncol = 3, nrow = 1, scales = "free"),
  
  
  
  
  
  
  
  #** Gaussian Outcome  ####
  #*** Prediction  ####
  sim_gaus_tab = make_sim_main_table(sim_success_rate,
                                     dist = "gaussian",
                                     measures = "R2"),
  
  sim_gaus_tab_latex = format_predict_tbls(
    sim_gaus_tab,
    caption = "The average and standard deviation of the out-of-sample $R^2$ measure for
    Gaussian outcomes over 50 iterations. The models of comparison include the proposed
    Bayesian hierarchical additive model (BHAM), linear LASSO model (LASSO), component
    selection and smoothing operator (COSSO), adaptive COSSO, mgcv, sparse Bayesian
    generalized additive model (SB-GAM), and spikeSlabGAM model. mgcv doesn't provide estimation
    when number of parameters exceeds sample size i.e. p = 100, 200.",
    label = "tab:gaus") %>% 
    cat(file = "Manuscript/Tabs/sim_gaus_tab.tex"),
  
  # Percentage of change

  sim_gaus_pred_bham_lasso = sim_pred_raw %>% filter(dist == "gaussian", !(p %in% c(4,10))) %>%
    compare_methods(method_1 = "bamlasso", method_2 = "SB_GAM", measures="R2") %>% 
    with(sprintf("%.0f%% (%.0f%%)", median*100, IQR*100)),
  
  
  #*** Bi-level Selection  ####
  sim_gaus_comp_select_raw = sim_comp_select_raw(sim_success_rate,
                                                 dist = "gaussian"),
  
  # plot_comp_select(sim_gaus_comp_select_raw[[5]]),
  
  # sim_gaus_comp_select_raw[[5]]$comp_select %>% filter(Variable %in% paste0("x",1:4)),
  
  #*** Variable Selection  ####
  sim_gaus_var_select_raw = sim_var_select_raw(sim_success_rate,
                                               dist = "gaussian"),
  # plot_var_select(sim_gaus_var_select_raw[[3]])
  sim_gaus_var_select_tbl = map_dfr(1:5, 
                                    ~make_sim_var_metric_raw(sim_gaus_var_select_raw[[.x]],
                                                             sim_gaus_comp_select_raw[[.x]])) %>%
    group_by(p, Method, Metric) %>%
    summarise(mean = mean(value, na.rm = TRUE)#, sd = sd(value, na.rm = TRUE)
    ) %>%
    pivot_wider(names_from = Method, values_from = mean) %>% 
    ungroup(),
  
  sim_gaus_var_select_tbl_latex = sim_gaus_var_select_tbl %>% 
    format_var_slct_tbls(caption = "The variable selection performance of Gaussian simulations,
                         measured by positive predictive value (precision), true positive rate (recall),
                         and Matthews correlation coefficient (MCC), for the high-dimensional methods
                         averaged over 50 iterations. The models of comparison include the proposed Bayesian
                         hierarchical additive model (BHAM), linear LASSO model (LASSO), component selection
                         and smoothing operator (COSSO), adaptive COSSO, sparse Bayesian generalized additive
                         model (SB-GAM), and spikeSlabGAM model. MCC is ill-defined when $p=4$ simulation
                         (no true negative), and hence omitted for all methods.", label = "tab:sim_gaus_var_select")%>% 
    cat(file = "Manuscript/Tabs/sim_gaus_var_slct_tab.tex"),
  
  #*** Time  ####
  sim_gaus_time_bham_lasso = sim_time_raw %>% filter(dist == "gaussian", ) %>%
    select(-c(user.self, sys.self, user.child, sys.child)) %>% 
    pivot_wider(names_from = method, values_from = elapsed) %>% 
    mutate(bamlasso = bamlasso_cv + bamlasso_final,
           SB_GAM = bai_cv + bai_final) %>% 
    select(it, dist, p, bamlasso, SB_GAM) %>% 
    pivot_longer(cols = c(bamlasso, SB_GAM), names_to = "method", values_to = "time") %>% 
    compare_methods(method_1 = "bamlasso", method_2 = "SB_GAM", measures="time") %>% 
    with(sprintf("%.0f%% (%.0f%%)", median*100, IQR*100)),
  
  
  
  #** Simulation Time ####
  sim_tim_tab = make_time_table(sim_success_rate),
  sim_tim_tab_latex = sim_tim_tab %>% 
    rename(Distribution = dist,
           P = p,
           COSSO = cosso,
           `Adaptive COSSO` = acosso) %>% 
    xtable(
      caption = "The average and standard deviation of computation time in seconds,
      including cross-validation and final model fitting, over 50 iterations. The models
      of comparison include the proposed Bayesian hierarchical additive model (BHAM), the linear LASSO model (LASSO),
      component selection and smoothing operator (COSSO), adaptive COSSO,
      mgcv, sparse Bayesian generalized additive model (SB-GAM), spikeSlabGAM. mgcv doesn't 
      provide estimation when number of parameters exceeds sample size i.e. p = 100, 200.",
      label = "tab:time_sim",
      booktabs = TRUE
    ) %>% 
    print(comment = FALSE, include.rownames = FALSE,
          floating.environment = "sidewaystable",# caption.placement = "top"
          table.placement = "!h"
    ) %>% 
    cat(file = "Manuscript/Tabs/sim_time_tab.tex"),
  
  
  #* Linear Simulation Study ####
  tar_files(sim_lnr_path,
            list.files("/data/user/boyiguo1/bgam/sim_res/main_lnr", recursive = T, full.names = T)),
  sim_lnr_success_rate = create_success_rate_table(sim_lnr_path),
  sim_lnr_mdl_fail = create_mdl_fail_rate(sim_lnr_success_rate),
  #** Binomial Outcome  ####
  #*** Prediction  ####
  sim_lnr_binom_tab = make_sim_main_table(sim_lnr_success_rate,
                                          dist = "binomial",
                                          measures = "auc"),
  sim_lnr_binom_tab_latex = format_predict_tbls(
    sim_lnr_binom_tab, 
    # TODO: edit the caption
    caption = "The average and standard deviation of the out-of-sample area under the curve measures
    for binomial outcomes over 50 iterations. The models of comparison include the proposed
    Bayesian hierarchical additive model (BHAM), linear LASSO model (LASSO), component
    selection and smoothing operator (COSSO), adaptive COSSO, mgcv, sparse Bayesian
    generalized additive model (SB-GAM), and spikeSlabGAM model. mgcv doesn't provide
    estimation when number of parameters exceeds sample size i.e. p = 100, 200.",
    label = "tab:lnr_bin_auc")%>% 
    cat(file = "Manuscript/Tabs/sim_lnr_binom_tab.tex"),
  
  #*** Bi-level Selection  ####
  sim_lnr_binom_comp_select_raw = sim_comp_select_raw(sim_lnr_success_rate,
                                                      dist = "binomial"),
  
  # plot_comp_select(sim_lnr_binom_comp_select_raw[[2]]),
  
  #*** Variable Selection  ####
  sim_lnr_binom_var_select_raw = sim_var_select_raw(sim_lnr_success_rate,
                                                    dist = "binomial"),
  
  # Table Presentation of Var Selection
  sim_lnr_binom_var_select_tbl = map_dfr(1:5, 
                                         ~make_sim_var_metric_raw(sim_lnr_binom_var_select_raw[[.x]],
                                                                  sim_lnr_binom_comp_select_raw[[.x]])
  ) %>%
    group_by(p, Method, Metric) %>%
    summarise(mean = mean(value, na.rm = TRUE)#, sd = sd(value, na.rm = TRUE)
    ) %>%
    pivot_wider(names_from = Method, values_from = mean) %>% 
    ungroup(),
  
  sim_lnr_binom_var_select_tbl_latex = sim_lnr_binom_var_select_tbl %>% 
    format_var_slct_tbls(caption = "The variable selection performance of linear binomial simulations,
                         measured by positive predictive value (precision), true positive rate (recall),
                         and Matthews correlation coefficient (MCC), for the high-dimensional methods
                         averaged over 50 iterations. The models of comparison include the proposed Bayesian
                         hierarchical additive model (BHAM), linear LASSO model (LASSO), component selection
                         and smoothing operator (COSSO), adaptive COSSO, sparse Bayesian generalized
                         additive model (SB-GAM), and spikeSlabGAM model. MCC is ill-defined when $p=4$
                         simulation (no true negative), and hence omitted for all methods.",
                         label = "tab:sim_lnr_binom_var_select")%>% 
    cat(file = "Manuscript/Tabs/sim_lnr_binom_var_slct_tab.tex"),
  
  
  
  #** Gaussian Outcome  ####
  #*** Prediction  ####
  sim_lnr_gaus_tab = make_sim_main_table(sim_lnr_success_rate,
                                         dist = "gaussian",
                                         measures = "R2"),
  sim_lnr_gaus_tab_latex = format_predict_tbls(
    sim_lnr_gaus_tab, # TODO: edit the caption
    caption = "The average and standard deviation of the out-of-sample $R^2$ measure for
    Gaussian outcomes over 50 iterations. The models of comparison include the proposed
    Bayesian hierarchical additive model (BHAM), linear LASSO model (LASSO), component
    selection and smoothing operator (COSSO), adaptive COSSO, mgcv, sparse Bayesian
    generalized additive model (SB-GAM), and spikeSlabGAM model. mgcv doesn't provide estimation
    when number of parameters exceeds sample size i.e. p = 100, 200.",
    label = "tab:lnr_gaus") %>% 
    cat(file = "Manuscript/Tabs/sim_lnr_gaus_tab.tex"),
  
  #*** Bi-level Selection  ####
  sim_lnr_gaus_comp_select_raw = sim_comp_select_raw(sim_lnr_success_rate,
                                                     dist = "gaussian"),
  
  # plot_comp_select(sim_lnr_gaus_comp_select_raw[[5]]),
  
  #*** Variable Selection  ####
  sim_lnr_gaus_var_select_raw = sim_var_select_raw(sim_lnr_success_rate,
                                                   dist = "gaussian"),
  # Table Presentation of Var Selection
  sim_lnr_gaus_var_select_tbl = map_dfr(1:5, 
                                        ~make_sim_var_metric_raw(sim_lnr_gaus_var_select_raw[[.x]],
                                                                 sim_lnr_gaus_comp_select_raw[[.x]])
  ) %>%
    group_by(p, Method, Metric) %>%
    summarise(mean = mean(value, na.rm = TRUE)#, sd = sd(value, na.rm = TRUE)
    ) %>%
    pivot_wider(names_from = Method, values_from = mean) %>% 
    ungroup(),
  
  sim_lnr_gaus_var_select_tbl_latex = sim_lnr_gaus_var_select_tbl %>% 
    format_var_slct_tbls(caption = "The variable selection performance of linear Gaussian
                         simulations, measured by positive predictive value (precision),
                         true positive rate (recall), and Matthews correlation coefficient (MCC),
                         for the high-dimensional methods averaged over 50 iterations.
                         The models of comparison include the proposed Bayesian hierarchical additive model (BHAM),
                         linear LASSO model (LASSO), component selection and smoothing operator (COSSO),
                         adaptive COSSO, sparse Bayesian generalized additive model (SB-GAM), and spikeSlabGAM model.
                         MCC is ill-defined when $p=4$ simulation (no true negative), and hence omitted for all methods.", 
                         label = "tab:sim_lnr_gaus_var_select") %>% 
    cat(file = "Manuscript/Tabs/sim_lnr_gaus_var_slct_tab.tex"),
  
  
  
  
  #  Real Data Analysis -----------------------------------------------------
  # * Emory Card Biobank ####
  # ** Load Data ####
  tar_target(ECB_train_path,
             "Real_Data/Emory_Card_Biobank/Data/Analysis_data_first_cohort.csv",
             format = "file"),
  
  tar_target(ECB_train_dat,
             readr::read_csv(ECB_train_path)),
  
  # ** GAM Screening ####
  tar_target(ECB_var_screen,
             var_screen(ECB_train_dat)),
  
  tar_target(ECB_cov,
             ECB_train_dat %>% 
               select(all_of(ECB_var_screen)) %>% 
               data.matrix),
  
  tar_target(ECB_outcome,
             ECB_train_dat %>% pull(death3yr)),
  
  # ** BHAM --------------------------------------------------------------- 
  ECB_sm_df = data.frame(
    Var = ECB_cov %>% colnames,
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
                              ss = c(0.065 , 0.5)),
  
  ECB_bamlasso_insample_msr = measure.bh(ECB_bamlasso_fnl),
  ECB_bamlasso_var = bamlasso_var_selection(ECB_bamlasso_fnl),
  # TODO: Use cv.bh to get the measures.
  
  # *** Plot Non-Linear Functions ###
  # TODO: to generalize this for a list of plots.
  ECB_bamlasso_nonlnr = ECB_bamlasso_var$`Non-parametric`[[1]],
  
  tar_target(
    ECB_plot_list,
    plot_smooth_term(ECB_bamlasso_fnl, ECB_bamlasso_nonlnr, ECB_sm_obj$Smooth,
                     min = min(ECB_cov[, ECB_bamlasso_nonlnr])-0.1,
                     max = max(ECB_cov[, ECB_bamlasso_nonlnr]) + 0.1)+
      xlab(str_split(ECB_bamlasso_nonlnr, "[.]")[[1]][1])+
      theme_pubr()+
      theme(axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank()),
    pattern = map(ECB_bamlasso_nonlnr),
    iteration = "list"
  ),
  
  ECB_gg_plot = ggarrange(plotlist = ECB_plot_list) %>% 
    annotate_figure(left = "Linear Predictor", bottom = "Features"),
  
  ECB_plot = ggsave(
    filename = "Manuscript/Figs/ECB_plot.pdf",
    plot = ECB_gg_plot,
    device = "pdf"),
  
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
  
  ECB_SBGAM_fnl = SBGAM(X = ECB_cov, y = ECB_outcome, family = "binomial", lambda0 = 40, a = 1, b = 1),
  
  ECB_SBGAM_insample_msr = measure.glm(y = ECB_outcome, ECB_SBGAM_fnl$mu.pred, family = "binomial"),
  
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
  
  WLM_bamlasso_cv = tune.bgam(WLM_bamlasso_raw, s0 = seq(0.001, 0.1, 0.01)),
  
  
  WLM_bamlasso_fnl = bamlasso(WLM_bgam_dat$train_dat, WLM_train_dat$out_HOMA_std, family = "gaussian",
                              ss=c(0.001, 0.5),
                              group = WLM_bgam_dat$group),
  # measure.bh(WLM_bamlasso_fnl),
  WLM_bamlasso_var = bamlasso_var_selection(WLM_bamlasso_fnl),
  
  # *** Plot Non-Linear Functions ###
  # TODO: to generalize this for a list of plots.
  
  
  WLM_bamlasso_nonlnr = WLM_bamlasso_var$`Non-parametric`[[1]],
  
  tar_target(
    WLM_plot_list,
    plot_smooth_term(WLM_bamlasso_fnl, WLM_bamlasso_nonlnr, WLM_bgam_dat$sm$Smooth,
                     min = min(WLM_train_dat[, WLM_bamlasso_nonlnr])-0.1,
                     max = max(WLM_train_dat[, WLM_bamlasso_nonlnr]) + 0.1)+
      xlab(WLM_bamlasso_nonlnr)+
      theme_pubr()+
      theme(axis.title.y = element_blank()),
    pattern = map(WLM_bamlasso_nonlnr),
    iteration = "list"
  ),
  
  WLM_gg_plot = ggarrange(plotlist = WLM_plot_list) %>% 
    annotate_figure(left = "Linear Predictor", bottom = "Features"),
  
  WLM_plot = ggsave(
    filename = "Manuscript/Figs/WLM_plot.pdf",
    plot = WLM_gg_plot,
    device = "pdf"),
  
  
  
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
  
  #* Running Time ---------------------------------------------------------------
  # TODO: create a latex output
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
  
  # Manuscript --------------------------------------------------------------
  tar_files(manu_path,
            c("Manuscript/01-Intro.Rmd", "Manuscript/02-Method.Rmd",
              "Manuscript/03-Simulation.Rmd", "Manuscript/04-Real_Data.Rmd",
              "Manuscript/05-Discussion.Rmd", "Manuscript/bibfile.bib")
  ),
  
  tar_files(
    manu_tbs,
    list.files("Manuscript/Tabs/", full.names = TRUE)
  ),
  
  tar_files(
    manu_figs,
    list.files("Manuscript/Figs/", full.names = TRUE)
  ),
  
  
  tar_render(manu, "Manuscript/00-main.Rmd",
             output_file = "SS_GAM.pdf"),
  
  # R & R Letter --------------------------------------------------------------
  tar_files(RR_path,
            c("Manuscript/R&R/01-reviewer_1.Rmd", "Manuscript/R&R/02-reviewer_2.Rmd",
              "Manuscript/R&R/03-reviewer_4.Rmd", "Manuscript/R&R/reply_ref.bib")
  ),
  
  tar_render(RR, "Manuscript/R&R/00-response_letter.Rmd",
             output_file = "response_SS_GAM.pdf")
)

