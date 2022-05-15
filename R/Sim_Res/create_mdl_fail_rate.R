create_mdl_fail_rate <- function(dat){
   dat %>% filter(n_success!=0) %>% 
    pmap_dfr(.f = function(path, ...){
        list.files(path, full.names = TRUE) %>% 
        grep("\\.rds", ., value = TRUE)  %>% 
      map_dfr(.f = function(file){
        # browser()
       res <- readRDS(file)$test %>% select(deviance) %>% 
         rownames_to_column(var = "method") %>% 
         pivot_wider(names_from = "method", values_from = deviance)
      }) %>% 
        summarize(across(everything(), ~sum(is.na(.)))) %>% 
        tibble(..., .)
    })
}