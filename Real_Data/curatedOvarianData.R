library(tidyverse)
library(curatedOvarianData)


dat_lst <- data(package="curatedOvarianData")

#### Display event rate for all datasets in the package ####
data_brief <- map_dfr(dat_lst$results[,"Item"], .f = function(dat_name){
  
  eval(rlang::expr(data(!!dat_name)))
  
  dat <- eval(rlang::parse_expr(dat_name))
  
  peno_dat <- pData(dat)
  
  
  
  # remove loaded data
  eval(rlang::expr(rm(dat)))
  eval(rlang::expr(rm(!!dat_name)), envir = globalenv())
  
  return(table(peno_dat$vital_status) %>% c %>% t %>%
           data.frame(dataset = dat_name))
})
