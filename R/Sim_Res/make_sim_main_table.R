make_sim_main_table <- function(success_rate, dist, section = "test" , measures){
  # browser()
  
  total_dat <- success_rate %>% 
    # filter(dist==dist, n_success!=0) %>% 
    filter(dist=={{dist}}, n_success!=0) %>%
    pull(path) %>% 
    map_dfr( .f = function(sim){
      
      
      sim.df <- unglue_data(sim, 
                            "{}/bgam_{study}_-dis_{dist}-p_{p}") %>% 
        mutate( p = as.numeric(p))
      fls <- list.files(sim, full.names = TRUE) 
      fls <- fls[grep(".rds", x=fls)]
      n <- length(fls)
      
      
      if(n == 0)
        return(data.frame(NULL))
      
      # .file <- fls[1]
      
      ret <- fls %>%
        map_dfr(.f = function(.file){
          it <- unglue_data(.file, "{whatever}/it_{it}.rds") %>% pull(it) %>% as.numeric
          ret <- data.frame(
            it = it,
            read_rds(.file) %>% 
              data.frame() %>% 
              rownames_to_column("method")
            
          )
        }) %>%
        arrange(it) %>% 
        mutate(p = sim.df$p)#  %>% 
      
    }) %>% 
    mutate(method = factor(
      method,
      levels = c("bglm_t", "bglm_de", "blasso",
                 "bglm_t_group", "bglm_de_group", "blasso_group",
                 "bglm_spline_t", "bglm_spline_de", "blasso_spline",
                 "cosso", "acosso", "mgcv", "SB_GAM"
      )
    )
    )
  
  total_dat %>% 
    select(p, method, 
           {{measures}} := paste(section, measures, sep=".")) %>% #head
    filter(method %in% c("bglm_spline_de", "blasso_spline",
                         "cosso", "acosso", "mgcv", "SB_GAM")) %>% 
    group_by(p, method) %>% 
    summarize(
      across({{measures}}, 
             list(mean = mean, sd = sd), na.rm = T,
             .names = "{.fn}")#,
              # n_miss = sum(is.na(auc))
    ) %>% 
    ungroup() %>% 
    transmute(
      p, method,
      {{measures}} := sprintf("%.2f (%.2f)", mean, sd)#,
      # misclass = sprintf("%.2f (%.2f)", misclass_mean, misclass_sd)
    ) %>% 
    pivot_wider(id_cols = "p", names_from = "method",
                values_from = {{measures}})
    # pivot_longer(cols = starts_with("test.")) %>%
    # mutate(name = str_remove(name, pattern = fixed("test."))) %>% 
    # rename_at(vars(dplyr::starts_with("test.")), ~str_remove(., pattern = fixed("test."))) %>%
    # # select(p,method, {{measures}}) %>% 
    # mutate(p = factor(p)) %>% 
    # group_by(p, method) %>% 
    # dplyr::summarise(across(everything(),list(mean = mean, sd = sd), na.rm = TRUE, .names = "{.col}.{.fn}")) %>% 
    # ungroup
}
