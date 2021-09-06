summary_cv_SBGAM <- function(cv.SBGAM_output, family = "gaussian"){
  map_dfr(1:length(cv.SBGAM_output$lambda0),
           .f = function(i){
             measure.glm(cv.SBGAM_output$y.new, cv.SBGAM_output$prevalid.mu[,i], family = family) %>% 
               t %>% data.frame()
           }) %>% 
     data.frame(lambda = cv.SBGAM_output$lambda0,.)
}


summary_cv_SBGAM_per_fold <- function(cv.SBGAM_output, family = "gaussian"){
  browser()
  ret <- map_dfr(1:length(cv.SBGAM_output$lambda0),
          .f = function(i){
            map_dfr(unique(cv.SBGAM_output$folds), .f = function(j){
              idx <- which(cv.SBGAM_output$folds==j)
              .y <- cv.SBGAM_output$y.new[idx]
              pred.y <- cv.SBGAM_output$prevalid.mu[idx,i]
              measure.glm(.y, pred.y, family = family) %>% 
                t %>% data.frame()
            }, .id = "folds")
          }, .id = "lambda_idx") %>% 
    mutate(
      lambda0 = cv.SBGAM_output$lambda0[lambda_idx %>% as.numeric] %>% factor
    )
  # ggplot(data = ret) +
  #   geom_violin(aes(x = lambda0, y = auc))
  return(ret)
  
  # %>% 
  #   data.frame(lambda = cv.SBGAM_output$lambda0,.)
}