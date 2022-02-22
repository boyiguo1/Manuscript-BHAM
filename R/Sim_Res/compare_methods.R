compare_methods <- function(dat, method_1, method_2, measures){
  # browser()
  if(!all(c(method_1, method_2) %in% unique(dat$method)))
    stop("method_1, method_2 don't exist in data. Check spelling")
  

  
  tmp <- dat %>% filter(
    method %in% c(method_1, method_2)
  ) %>% 
  select(p, dist, it, method, all_of({{measures}}))
  
  if(tmp %>% 
     pull(all_of({{measures}})) %>% 
     is.na() %>% 
     all())
  stop("Meausure does not exist in data")
  
  tmp %>% pivot_wider(
    names_from = method,
    values_from = {{measures}}
  ) %>% 
    rename(m1 := {{method_1}},
           m2 := {{method_2}}) %>% 
    mutate(ratio = (m1-m2)/m2) %>%
    summarize(across(ratio, list(median = median, IQR = IQR), .names="{.fn}"))
  
   
}