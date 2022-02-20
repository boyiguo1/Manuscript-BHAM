args=(commandArgs(TRUE))

if(length(args)==0){
  print("No arguments supplied.")
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

# Arguments
# n_train <- 500
# n_test <- 1000
# p <- 5
# it <- 10
# dis <- "gaussian"
# # dis <- "binomial"


# Required Library
library(mgcv)
library(splines)
library(BhGLM)
library(BHAM)
library(tidyverse)
library(rlang)
library(cosso)
library(sparseGAM)    # Bai(2021)
library(glmnet)
library(spikeSlabGAM)


# Helper functions
source("~/GitHub/Manuscript-BHAM/Simulation/Code/helper_func.R")

# Simulation Fixed Parameters and Functions
source("~/GitHub/Manuscript-BHAM/Simulation/Code/sim_pars_funs.R")

# Using Array ID as seed ID
it <- Sys.getenv('SLURM_ARRAY_TASK_ID') %>% as.numeric
# it <- 1

set.seed(it)
fam_fun <- dis()  
# fam_fun <- str2expression(paste0(dis,"()")) %>% eval

x_all <- sim.x(n=n_total, m=p, corr=0)
yy <-  sim.y(x = x_all[, 1:4], mu = 0, coefs = c(0.362, 0.395, -0.418, -0.431))


dat_all <- with(data = yy,
                data.frame(x_all,
                           # eta = eta,
                           y = if(fam_fun$family=="gaussian") y.normal else y.ordinal)
)

train_eta <- yy$eta[1:n_train, ]
test_eta <- yy$eta[(n_train+1):n_total, ]

dat <- dat_all[1:n_train, ] %>% data.frame
test_dat <- dat_all[(n_train+1):n_total, ] %>% data.frame

# # Train Data
# tmp <- sim_Bai(n_train, p, family = fam_fun)
# dat <- tmp$dat %>% data.frame
# theta <- tmp$theta
# 
# # Test Data
# test_tmp <- sim_Bai(n_test, p, family = fam_fun)
# test_dat <- test_tmp$dat %>% data.frame
# test_theta <- tmp$theta


mgcv_df <- data.frame(
  Var = setdiff(names(dat), "y"),
  Func = "s",
  Args = paste0("bs='cr', k=", K)
)


#### Linear Lasso ####
lasso_mdl <- cv.glmnet(x = data.matrix(dat %>% select(-y)),
                       y = data.matrix(dat %>% pull(y)),
                       nfolds = 5, family = fam_fun$family
                      )
lasso_fnl_mdl <- glmnet(x = data.matrix(dat %>% select(-y)),
                        y = data.matrix(dat %>% pull(y)),
                        family = fam_fun$family, lambda = lasso_mdl$lambda.min)

# predict(lasso_fnl_mdl, type = "nonzero")

lasso_train <- measure.glm(dat$y, predict(lasso_mdl, newx = data.matrix(dat %>% select(-y)), type = "response", s = "lambda.min"), family = fam_fun$family)
lasso_test <- measure.glm(test_dat$y, predict(lasso_mdl, newx = data.matrix(test_dat %>% select(-y)), type = "response", s = "lambda.min"), family = fam_fun$family)

lasso_vs <- ((lasso_fnl_mdl$beta %>% as.vector())!=0) %>% `names<-`(setdiff(names(dat), "y"))

#### mgcv_mdl ####

mgcv_time <- system.time({
mgcv_mdl <- tryCatch({
  gam(create_HD_formula(y~1, spl_df = mgcv_df), 
      data=dat %>% data.frame(), family=fam_fun)
},
error = function(err) {
  mgcv_mdl <- NULL
  return(NULL)
}
)
})
# mgcv_end <- Sys.time()

# mgcv_time <- mgcv_end - mgcv_start


mgcv_sum <- NULL
mgcv_train <- make_null_res(fam_fun$family)
mgcv_test <- make_null_res(fam_fun$family)
mgcv_vs <- rep(NA, length(lasso_vs)) %>% `names<-`(setdiff(names(dat), "y"))


if(!is.null(mgcv_mdl)){
  mgcv_vs <- (summary(mgcv_mdl)$s.table[,"p-value"] < 0.05) %>% `names<-`(setdiff(names(dat), "y"))
  
  
  mgcv_train <- measure.glm(mgcv_mdl$y, predict(mgcv_mdl, type = "response"), family = fam_fun$family)
  
  
  mgcv_pred <- predict(mgcv_mdl, newdata=test_dat, type = "response")
  mgcv_test <- measure.glm(test_dat$y, mgcv_pred, family = fam_fun$family)
}



#### BGAMs ####
train_sm_dat <- BHAM::construct_smooth_data(mgcv_df, dat)
train_smooth <- train_sm_dat$Smooth
train_smooth_data <- train_sm_dat$data
test_sm_dat <- BHAM::make_predict_dat(train_sm_dat$Smooth, dat = test_dat)


#### Fit BH Models ####

func_list <- list(
  # "bglm", "bglm", "bmlasso", 
  # "bglm", "bglm", "bmlasso",
  # "bgam", 
  # "bgam",
  "bamlasso"#,
  # bgam
)

group_list <- list(
  # NULL, NULL, NULL, 
  # make_group(names(train_smooth_data)), make_group(names(train_smooth_data)), make_group(names(train_smooth_data)),
  # make_group(names(train_smooth_data)), 
  # make_group(names(train_smooth_data)), 
  make_group(names(train_smooth_data))#,
  # make_group(names(train_smooth_data))
)

prior <- list(
  # mt(df=Inf), mde(), NULL,
  # mt(df=Inf), mde(), NULL,
  # mt(df=Inf),
  # mde(), 
  NULL#,
  # mt(df=Inf)
)

# s0_seq <- seq(0.005, 0.1, 0.005)
# if(p==4){
#   s0_seq <- seq(0.04, 1, 0.05); names(s0_seq) <- s0_seq
# }
# .fun <- "bamlasso"
# .group <- make_group(names(train_smooth_data))

mdls <- pmap(list(func_list, group_list, prior), .f = function(.fun, .group, .prior){
  cv_time <- system.time({
    if(.fun == "bmlasso" || .fun == "bamlasso"){
      
      mdl <- call(.fun, x = train_smooth_data, y = dat$y, family = fam_fun$family, group = .group) %>% eval
      cv_res <- tune.bgam(mdl, nfolds = 5, s0= bamlasso_seq, verbose = FALSE)
    } else {
      mdl <- call(.fun, formula = y~.-y,
                  data = data.frame(train_smooth_data, y = dat$y), family = fam_fun$family,
                  prior=.prior, group = .group) %>% eval
      
      cv_res <- tune.bgam(mdl, nfolds = 5, s0= bgam_seq, verbose = FALSE)
    }
  })
  
  
  # mdl <- call(.fun, x = train_smooth_data, y = dat$y, family = fam_fun$family, group = .group,
  #             ss = c(0.0011, 0.5)) %>% eval
  
  
  s0_min <- cv_res$s0[which.min(cv_res$deviance)]
  
  final_time <- system.time({
    if(.fun == "bmlasso" || .fun == "bamlasso"){
      mdl <- call(.fun, x = train_smooth_data, y = dat$y, family = fam_fun$family, group = .group,
                  ss = c(s0_min, 0.5)) %>% eval
      
    } else {
      tmp_prior <- if(.prior$prior == "mt") mt(s0 = s0_min,df = Inf)
      else if(.prior$prior == "mde") mde(s0 = s0_min)
      mdl <- call(.fun, formula = y~.-y,
                  data = data.frame(train_smooth_data, y = dat$y), family = fam_fun$family,
                  prior=tmp_prior, group = .group) %>% eval
    }
  })
  
  
  return(list(mdl = mdl, cv_time = cv_time, final_time = final_time))
})

names(mdls) <- c(
  # "bglm_t", "bglm_de", "blasso",
  # "bglm_t_group", "bglm_de_group", "blasso_group",
  # "bglm_spline_t", 
  # "bgam",
  "bamlasso"#,
  # "bglm_share_t"
)


bham_time <- imap_dfr(mdls, .f = function(.mdl, .y){
  # browser()
  # suppressMessages({
  ret <- rbind(
    .mdl$cv_time %>% print %>% c,
    .mdl$final_time %>% print %>% c
  ) 
  # })
  
  rownames(ret) <- paste(.y, c("cv", "final"), sep = "_")
  
  return(data.frame(ret))
})


# TODO: insert here
# 1) find which one is the bamlasso.
bamlasso_vs_part <- bamlasso_var_selection(mdls[["bamlasso"]]$mdl)
bamlasso_vs <- rep(FALSE, length(lasso_vs)) %>% `names<-`(setdiff(names(dat), "y"))
bamlasso_vs[bamlasso_vs_part$`Non-parametric`$Variable] <- TRUE



train_res <- map_dfr(mdls, .f = function(.mdl){
  
  measure.bh(.mdl$mdl)
  # if("glmnet" %in% class(.mdl))
  # measure.glm(.mdl$y, .mdl$linear.predictors, family = fam_fun$family)
  # else 
  # measure.glm(.mdl$y, .mdl$fitted.values, family = fam_fun$family)
},.id = "mdl") %>% 
  column_to_rownames("mdl")


test_res <- map_dfr(mdls, .f = function(.mdl){
  if("glmnet" %in% class(.mdl$mdl))
    measure.bh(.mdl$mdl, test_sm_dat, test_dat$y)
  else{
    pred <- predict.glm(.mdl$mdl, newdata = test_sm_dat, type = "response")
    measure.glm(test_dat$y, pred, family = fam_fun$family) 
  }
},.id = "mdl") %>% 
  column_to_rownames("mdl")


#### Fit COSSO Models ####
cosso.time <- system.time({
  cosso.mdl <- tryCatch({
    cosso(dat[,-ncol(dat)], dat$y, family = str_to_title(fam_fun$family), scale = F, nbasis=K)
  },
  error = function(err) {
    cosso.mdl <- NULL
    return(NULL)
  }
  )
  
  if(!is.null(cosso.mdl)){
    tn_mdl <- tryCatch({
      tune.cosso(cosso.mdl)
    },
    error = function(err) {
      return(NULL)
    }
    )
  }
})

cosso_vs <- rep(NA, length(lasso_vs)) %>% `names<-`(setdiff(names(dat), "y"))
if(!is.null(cosso.mdl)){  
  cosso_vs <- rep(FALSE, length(lasso_vs)) %>% `names<-`(setdiff(names(dat), "y"))
  cosso_vs[predict.cosso(cosso.mdl, M=ifelse(!is.null(tn_mdl), tn_mdl$OptM, 2), type = "nonzero")] <- TRUE
  
  cosso.train.res <- predict.cosso(cosso.mdl, xnew=dat[,-ncol(dat)],M=ifelse(!is.null(tn_mdl), tn_mdl$OptM, 2), type = "fit")
  cosso_train_msr <- measure.glm(cosso.mdl$y, fam_fun$linkinv(cosso.train.res), family = fam_fun$family)
  
  cosso.test.res <- predict.cosso(cosso.mdl, xnew=test_dat[,-ncol(test_dat)], M=ifelse(!is.null(tn_mdl), tn_mdl$OptM, 2), type = "fit")
  cosso_test_msr <- measure.glm(test_dat$y, fam_fun$linkinv(cosso.test.res), family = fam_fun$family) 
} else{
  cosso_train_msr <- make_null_res(fam_fun$family)
  cosso_test_msr <- make_null_res(fam_fun$family)
  cosso_vs <- rep(NA, length(lasso_vs))
}


#### Fit ACOSSO Models ####


acosso.time <- system.time({
  acosso.mdl <- tryCatch({
    
    Binomial.wt <- SSANOVAwt(dat[,-ncol(dat)], dat$y,family=str_to_title(fam_fun$family), nbasis=K)
    cosso(dat[,-ncol(dat)], dat$y, wt= Binomial.wt, family = str_to_title(fam_fun$family), scale = F, nbasis=K)
  },
  error = function(err) {
    acosso.mdl <- NULL
    return(NULL)
  }
  )
  
  if(!is.null(acosso.mdl)){
    tn_amdl <- tryCatch({
      tune.cosso(acosso.mdl)
    },
    error = function(err) {
      return(NULL)
    }
    )
  }
  
})


acosso_vs <- rep(NA, length(lasso_vs)) %>% `names<-`(setdiff(names(dat), "y"))
if(!is.null(acosso.mdl)){  
  acosso_vs <- rep(FALSE, length(lasso_vs)) %>% `names<-`(setdiff(names(dat), "y"))
  acosso_vs[predict.cosso(acosso.mdl, M=ifelse(!is.null(tn_amdl), tn_amdl$OptM, 2), type = "nonzero")] <- TRUE
  acosso.train.res <- predict.cosso(acosso.mdl, xnew=dat[,-ncol(dat)],
                                    M=ifelse(!is.null(tn_amdl), tn_amdl$OptM, 2), type = "fit")
  acosso_train_msr <- measure.glm(acosso.mdl$y, fam_fun$linkinv(acosso.train.res), family = fam_fun$family)
  
  acosso.test.res <- predict.cosso(acosso.mdl, xnew=test_dat[,-ncol(test_dat)],
                                   M=ifelse(!is.null(tn_amdl), tn_amdl$OptM, 2), type = "fit")
  acosso_test_msr <- measure.glm(test_dat$y, fam_fun$linkinv(acosso.test.res), family = fam_fun$family) 
} else{
  acosso_train_msr <- make_null_res(fam_fun$family)
  acosso_test_msr <- make_null_res(fam_fun$family)

}
#### Fit SB-GAM Models ####

bai_cv_time <- system.time({cv_bai_mdl <- cv.SBGAM(y = dat$y, X = dat[,-ncol(dat)], df=K, family=fam_fun$family, a = 1, b = 1,
                                                   max.iter=100, tol = 1e-6)})


bai_final_time <- system.time({bai_mdl <- SBGAM(dat$y, dat[,-ncol(dat)], X.test = test_dat[,-ncol(test_dat)], df=K, family=fam_fun$family,
                                                lambda0 = cv_bai_mdl$lambda0.min, a = 1, b = 1,
                                                max.iter=100, tol = 1e-6, print.iter=FALSE)})

bai_vs <- as.logical(bai_mdl$classifications) %>% `names<-`(setdiff(names(dat), "y"))
bai_train_msr <- make_null_res(fam_fun$family)
bai_test_msr <- measure.glm(test_dat$y, bai_mdl$mu.pred, family = fam_fun$family) 


# SpikeSlabGAM ------------------------------------------------------------

options(mc.cores = 1)

ssGAM_formula <- grep("x", names(dat), value = TRUE) %>% 
  map_chr(.f = function(var_name){
    glue::glue("lin({var_name}) + sm({var_name}, K={K})")
  }) %>% 
  paste(collapse = " + ") %>% 
  paste0("y ~ ", .)

ssGAM_train_dat <- dat
# ssGAM_test_dat <- test_dat

if(fam_fun$family == "gaussian"){
  # Rescale the y
  ssGAM_train_dat <-  ssGAM_train_dat %>% 
    mutate(y = scale(y))
  
} 

rescale_ssGAM_pred <- function(response, family, y){
  if(family == "gaussian"){
    response <- attr(y, "scaled:center") + response*attr(y, "scaled:scale")
  }
  
  return (response)
}

ssGAM_time <- system.time({
  ssGAM_mdl <- spikeSlabGAM(formula = as.formula(ssGAM_formula), data = ssGAM_train_dat, family=fam_fun$family)
})



ssGAM_vs <- (summary(ssGAM_mdl)$trmSummary) %>% data.frame() %>% rownames_to_column("part") %>% 
select(part, prob = `P.gamma...1.`) %>% 
  filter(str_detect(part, "x")) %>% 
  unglue::unglue_unnest(part, "{}({var})", remove = FALSE) %>% 
  group_by(var) %>% 
  summarize(ssGAM = any(prob>0.5)) %>% 
  column_to_rownames("var") %>% as.vector


ssGAM_vs_part <- (summary(ssGAM_mdl)$trmSummary) %>% data.frame() %>% rownames_to_column("part") %>% 
  select(part, prob = `P.gamma...1.`) %>% 
  filter(str_detect(part, "x")) 

ssGAM_train_msr <- measure.glm(dat$y, 
                               predict(ssGAM_mdl, type = "response") %>% rescale_ssGAM_pred(family = fam_fun$family, y=ssGAM_train_dat$y),
                               family = fam_fun$family)
ssGAM_test_msr <- measure.glm(test_dat$y, 
                              predict(ssGAM_mdl, newdata = test_dat, type = "response") %>% rescale_ssGAM_pred(family = fam_fun$family, y=ssGAM_train_dat$y),
                              family = fam_fun$family)

#### Result Reporting ####
ret <- list(
  train = rbind(lasso = lasso_train,
                mgcv = mgcv_train,
                cosso = cosso_train_msr,
                acosso = acosso_train_msr,
                train_res,
                SB_GAM = bai_train_msr,
                ssGAM = ssGAM_train_msr),
  test = rbind(lasso = lasso_test,
               mgcv = mgcv_test,
               cosso = cosso_test_msr,
               acosso = acosso_test_msr,
               test_res,
               SB_GAM = bai_test_msr,
               ssGAM = ssGAM_test_msr),
  var_sel = data.frame(
    lasso = lasso_vs,
    mgcv = mgcv_vs,
    bamlasso = bamlasso_vs,
    cosso = cosso_vs,
    acosso = acosso_vs,
    SB_GAM = bai_vs,
    ssGAM = ssGAM_vs),
  
  var_part = list(
    bamlasso = bamlasso_vs_part,
    ssGAM = ssGAM_vs_part
  ),
  
  time = rbind(mgcv = mgcv_time %>% print %>% c,
               # bgam_global = bgam_global_time,
               # bgam_local = bgam_local_time,
               # bgam_share = bgam_shared_time,
               bham_time,
               cosso = cosso.time %>% print %>% c,
               acosso = acosso.time %>% print %>% c,
               bai_cv = bai_cv_time %>% print %>% c,
               bai_final = bai_final_time %>% print %>% c,
               ssGAM = ssGAM_time %>% print %>% c)
)


job_name <- Sys.getenv('SLURM_JOB_NAME')
saveRDS(ret, 
        paste0("/data/user/boyiguo1/bgam/sim_res/main_lnr/", job_name,"/it_",it,".rds"))


