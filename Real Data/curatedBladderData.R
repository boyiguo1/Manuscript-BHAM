# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("curatedBladderData")

library(curatedBladderData)
library(tidyverse)


dat_lst <- data(package="curatedBladderData")

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








dat <- GSE32894_eset
pheno_dat <- pData(dat) %>%
  select(-uncurated_author_metadata) %>% 
  mutate_if(is.logical, factor) %>% 
  mutate_if(is.character, factor) %>% 
  janitor::remove_constant() 
summary(pheno_dat)


# TODO: write aa function that display the event rate "death rate" for all the datasets