args=(commandArgs(TRUE))

# print(args)

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
# p <- 50
# it <- 10

# Required Library
library(mgcv)
library(splines)
library(BhGLM)
library(tidyverse)
library(rlang)
library(cosso)
# library(HRW)

source("~/bgam_nosharing/Code/helper_func.R")

# Using Array ID as seed ID
it <- Sys.getenv('SLURM_ARRAY_TASK_ID') %>% as.numeric
# it <- 1

set.seed(it)


# Train Data
tmp <- sim_Bai_logistic(n_train, p)
dat <- tmp$dat %>% data.frame
theta <- tmp$theta

# Test Data
test_tmp <- sim_Bai_logistic(n_test, p)
test_dat <- test_tmp$dat %>% data.frame
test_theta <- test_tmp$theta


mgcv_df <- data.frame(
  Var = setdiff(names(dat), "y"),
  Func = "s",
  Args ="bs='cr', k=27"
)


#### mgcv_mdl ####

mgcv_start <- Sys.time()
mgcv_mdl <- tryCatch({
  gam(create_HD_formula(y~1, spl_df = mgcv_df), 
      data=dat %>% data.frame(), family=binomial)
},
error = function(err) {
  mgcv_mdl <- NULL
  return(NULL)
}
)
mgcv_end <- Sys.time()

mgcv_time <- mgcv_end - mgcv_start


mgcv_sum <- NULL
mgcv_train <- data.frame(deviance = NA, auc = NA, mse = NA, mae = NA, misclassification = NA)
mgcv_test <- data.frame(deviance = NA, auc = NA, mse = NA, mae = NA, misclassification = NA)


if(!is.null(mgcv_mdl)){
  mgcv_sum <- summary(mgcv_mdl)$s.table[,"p-value"]
  
  mgcv_train <- measure.glm(mgcv_mdl$y, mgcv_mdl$fitted.values, family = binomial())
  
  
  mgcv_pred <- predict(mgcv_mdl, newdata=test_dat, type = "response")
  mgcv_test <- measure.glm(test_dat$y, mgcv_pred, family = binomial())
}



#### BGAMs ####
train_sm_dat <- Construct_Smooth_Data(mgcv_df, dat)
train_smooth <- train_sm_dat$Smooth
train_smooth_data <- train_sm_dat$data
test_sm_dat <- make_predict_dat(train_sm_dat$Smooth, dat = test_dat)


#### Fit BH Models ####

func_list <- list(bglm, bglm, bmlasso, bglm, bglm, bmlasso,
                  bglm_spline)
group_list <- list(NULL, NULL, NULL, 
                   make_group(names(train_smooth_data)), make_group(names(train_smooth_data)), make_group(names(train_smooth_data)),
                   make_group(names(train_smooth_data)))
prior <- list(mt(df=Inf), mde(), NULL,
              mt(df=Inf), mde(), NULL,
              mt(df=Inf))


mdls <- pmap(list(func_list, group_list, prior), .f = function(.fun, .group, .prior){
  if(identical(.fun , bmlasso))
    .fun(x = train_smooth_data, y = dat$y, family = "binomial", group = .group)
  else
    .fun(y~.-y,
         data = data.frame(train_smooth_data, y = dat$y), family = binomial(), prior=.prior,
         group = .group)
})

names(mdls) <- c("bglm_t", "bglm_de", "blasso",
                 "bglm_spline_t", "bglm_spline_de", "blasso_spline",
                 "bglm_share_t")

# tmp <- bmlasso(x = train_smooth_data, y = dat$y, family = "binomial", offset = NULL)

train_res <- map_dfr(mdls, .f = function(.mdl){
  
  if("glmnet" %in% class(.mdl))
    measure.glm(.mdl$y, .mdl$linear.predictors, family = binomial())
  else 
    measure.glm(.mdl$y, .mdl$fitted.values, family = binomial())
},.id = "mdl") %>% 
  column_to_rownames("mdl")


test_res <- map_dfr(mdls, .f = function(.mdl){
  if("glmnet" %in% class(.mdl))
    measure.bh(.mdl, test_sm_dat, test_dat$y)
  else{
    pred <- predict.glm(.mdl, newdata = test_sm_dat, type = "response")
    measure.glm(test_dat$y, pred, family = binomial()) 
  }
},.id = "mdl") %>% 
  column_to_rownames("mdl")



#### Fit bmlaso_spline Model ####
EM_CD <- bmlasso_spline(x = train_smooth_data, y = dat$y, family = "binomial", group = make_group(names(train_smooth_data)))
measure.glm(EM_CD$y, EM_CD$linear.predictors, family = binomial())
measure.bh(EM_CD, test_sm_dat, test_dat$y)

#### Fit COSSO Models ####

cosso.mdl <- tryCatch({
  cosso(dat[,-ncol(dat)], dat$y, family = "Binomial", scale = F, nbasis=25)
},
error = function(err) {
  cosso.mdl <- NULL
  return(NULL)
}
)

if(!is.null(cosso.mdl)){
  tn_mdl <- tryCatch({
    tune.cosso(acosso.mdl)
  },
  error = function(err) {
    return(NULL)
  }
  )

  cosso.train.res <- predict.cosso(cosso.mdl, xnew=dat[,-ncol(dat)],M=ifelse(!is.null(tn_mdl), tn_mdl$OptM, 2), type = "fit")
  cosso_train_msr <- measure.glm(cosso.mdl$y, cosso.train.res, family = binomial())
  
  cosso.test.res <- predict.cosso(cosso.mdl, xnew=test_dat[,-ncol(test_dat)], M=ifelse(!is.null(tn_mdl), tn_mdl$OptM, 2), type = "fit")
  cosso_test_msr <- measure.glm(test_dat$y, cosso.test.res, family = binomial()) 
} else{
  cosso_train_msr <- data.frame(deviance = NA, auc = NA, mse = NA, mae = NA, misclassification = NA)
  cosso_test_msr <- data.frame(deviance = NA, auc = NA, mse = NA, mae = NA, misclassification = NA)
}


#### Fit ACOSSO Models ####



acosso.mdl <- tryCatch({
  
  Binomial.wt <- SSANOVAwt(dat[,-ncol(dat)], dat$y,family="Bin", nbasis=25)
  cosso(dat[,-ncol(dat)], dat$y, wt= Binomial.wt, family = "Binomial", scale = F)
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
  
  
  
  acosso.train.res <- predict.cosso(acosso.mdl, xnew=dat[,-ncol(dat)],
                                    M=ifelse(!is.null(tn_amdl), tn_amdl$OptM, 2), type = "fit")
  acosso_train_msr <- measure.glm(acosso.mdl$y, acosso.train.res, family = binomial())
  
  acosso.test.res <- predict.cosso(acosso.mdl, xnew=test_dat[,-ncol(test_dat)],
                                   M=ifelse(!is.null(tn_amdl), tn_amdl$OptM, 2), type = "fit")
  acosso_test_msr <- measure.glm(test_dat$y, acosso.test.res, family = binomial()) 
} else{
  acosso_train_msr <- data.frame(deviance = NA, auc = NA, mse = NA, mae = NA, misclassification = NA)
  acosso_test_msr <- data.frame(deviance = NA, auc = NA, mse = NA, mae = NA, misclassification = NA)
}


ret <- list(
  train = rbind(mgcv = mgcv_train,
                cosso = cosso_train_msr,
                acosso = acosso_train_msr,
                train_res),
  test = rbind(mgcv = mgcv_test,
               cosso = cosso_test_msr,
               acosso = acosso_test_msr,
               test_res)
  # var_sel = rbind(mgcv = mgcv_sum,
  #                 bgam = bgam_sum,
  #                 bglm = bglm_sum),
  # time = rbind(mgcv = mgcv_time,
  #              bgam_global = bgam_global_time,
  #              bgam_local = bgam_local_time,
  #              bgam_share = bgam_shared_time)
)


job_name <- Sys.getenv('SLURM_JOB_NAME')
saveRDS(ret, 
        paste0("/data/user/boyiguo1/bgam_nosharing/Res/", job_name,"/it_",it,".rds"))


