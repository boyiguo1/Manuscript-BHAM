#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ful_dat
make_bgam_mdl <- function(dat) {
  
  mz_names <- dat %>% names
  mz_names <- mz_names[grep("mb_", mz_names, fixed = TRUE)] 
    
  
  sm_df <- data.frame(
    Var = mz_names,
    Func = "s",
    Args ="bs='cr', k=5"
  )

  sm_obj <- construct_smooth_data(sm_df, dat)

  dsn_mat <- sm_obj$data
  spl_pt_name <- names(dsn_mat)
  
  cov_mat <- model.matrix(~ -1 +cov_Age + cov_Sex + cov_Race + cov_Triglycerides_Baseline + cov_Weight_PC,
                          dat %>% select(starts_with("cov_")))
  dsn_mat <- data.frame(
    cov_mat,
    dsn_mat)
  
  spl_grp <- make_group(spl_pt_name)
  
  # The validation data doesn't overlap with training data
  # test_sm_dat <- make_predict_dat(sm_obj$Smooth, dat = valid_dat)
  
  # mdl_defaul <- bmlasso(dsn_mat, train_data$death3yr, family = "binomial", ss  = c(, 0.5))
  
  # bmlasso_mdl <- bmlasso(dsn_mat, dat$out_HOMA_PC, family = "gaussian",
  #                        group = make_group(spl_pt_name))
  
  spline_mdl <- bamlasso(dsn_mat, dat$out_HOMA_PC, family = "gaussian",
                         group = spl_grp)
  
  
  return(spline_mdl)
  
  
  # Training Data Performance
  # cat("bmlasso model\n")
  # measure.bh(mdl_defaul)
  # cat("bmlasso_spline model\n")
  # measure.bh(mdl_dflt)
  
  
  # test_sm_dat <- BHAM::make_predict_dat(sm_obj$Smooth, dat=test_data)
  # 
  # # Testing Data Performance
  # cat("bmlasso model\n")
  # measure.bh(mdl_defaul, new.x = test_sm_dat, new.y=test_data$death3yr)
  # cat("bmlasso_spline model\n")
  # measure.bh(mdl_dflt, new.x = test_sm_dat, new.y=test_data$death3yr)
  
  
  # Extremely Penalized Model
  
  # mdl_pen_extreme <- bamlasso(dsn_mat, dat$death3yr, family = "binomial",
  #                             group = make_group(names(dsn_mat)),
  #                             ss = c(0.001, 0.5))
  
  # spline_ext_cv_res <- cv.bh(mdl_pen_extreme, ncv = 10)
  
  # Training Data Performance
  # measure.bh(mdl_pen_extreme)
  
  
  # Testing Data Performance
  # measure.bh(mdl_pen_extreme, new.x = test_sm_dat, new.y=test_data$death3yr)

}
