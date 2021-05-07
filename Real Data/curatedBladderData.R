# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("curatedBladderData")

library(curatedBladderData)


data(package="curatedBladderData")

data("GSE32894_eset")

dat <- GSE32894_eset
pheno_dat <- pData(dat) %>%
  select(-uncurated_author_metadata) %>% 
  mutate_if(is.logical, factor) %>% 
  mutate_if(is.character, factor) %>% 
  janitor::remove_constant() 
summary(pheno_dat)


# TODO: write a function that display the event rate "death rate" for all the datasets