summary_cv_SBGAM <- function(cv.SBGAM_output, family = "gaussian"){
  map_dfr(1:length(cv.SBGAM_output$lambda0),
           .f = function(i){
             measure.glm(cv.SBGAM_output$y.new, cv.SBGAM_output$prevalid.mu[,i], family = family) %>% 
               t %>% data.frame()
           }) %>% 
     data.frame(lambda = cv.SBGAM_output$lambda0,.)
}