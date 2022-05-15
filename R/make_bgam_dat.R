make_bgam_dat <- function(train_dat, test_dat = NULL){
  
  # Create sm dat -----------------------------------------------------------
  mz_names <- train_dat %>% names
  mz_names <- mz_names[grep("mb_", mz_names, fixed = TRUE)] 
  
  
  sm_df <- data.frame(
    Var = mz_names,
    Func = "s",
    Args ="bs='cr', k=5"
  )
  
  sm_obj <- construct_smooth_data(sm_df, train_dat)
  
  dsn_mat <- sm_obj$data
  spl_pt_name <- names(dsn_mat)
  
  cov_mat <- model.matrix(~ -1 +cov_Age + cov_Sex + cov_Race + cov_Triglycerides_Baseline + cov_Weight_PC,
                          train_dat %>% select(starts_with("cov_")))
  dsn_mat <- data.frame(
    # cov_mat,
    dsn_mat)
  
  spl_grp <- make_group(spl_pt_name)
  
  ret <- list(train_dat = dsn_mat,
              group = spl_grp,
              sm = sm_obj)
  
  # Create test dat ---------------------------------------------------------
  if(!is.null(test_dat)){
    
    test_sm_dat <- make_predict_dat(sm_obj$Smooth, dat = test_dat)
    test_cov_mat <- model.matrix(~ -1 +cov_Age + cov_Sex + cov_Race + cov_Triglycerides_Baseline + cov_Weight_PC,
                                 test_dat %>% select(starts_with("cov_")))
    test_dsn_mat <- data.frame(
      # test_cov_mat,
      test_sm_dat
    )
    
    ret$test_dat <- test_dsn_mat
  }
  
  return(ret)
}