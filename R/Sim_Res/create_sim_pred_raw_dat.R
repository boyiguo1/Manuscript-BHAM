create_raw_data <- function(success_rate, section = "test"){
  # browser()
  
  total_dat <- success_rate %>% 
    filter( n_success!=0) %>%
    pull(path) %>% 
    map_dfr( .f = function(sim){
      # browser()
      sim.df <- unglue_data(sim, 
                            "{}/{}-dis_{dist}-p_{p}") %>% 
        mutate(p = as.numeric(p))
      
      # fls <- 
      list.files(sim, full.names = TRUE) %>% 
        grep(".rds", x=., value = TRUE) %>%
        map_dfr(.f = function(.file){
          # browser()
          it <- unglue_data(.file, "{whatever}/it_{it}.rds") %>% pull(it) %>% as.numeric
          ret <- data.frame(
            it = it,
            read_rds(.file) %>% 
              `[[`(section)%>% 
              data.frame() %>% 
              rownames_to_column("method")
            
          )
        }) %>%
        arrange(it) %>% 
        mutate(dist = sim.df$dist,
               p = sim.df$p)
    }) %>% 
    mutate(method = factor(
      method#,
      # levels = c("bglm_t", "bglm_de", "blasso",
      #            "bglm_t_group", "bglm_de_group", "blasso_group",
      #            "bglm_spline_t", "bglm_spline_de", "blasso_spline",
      #            "cosso", "acosso", "mgcv", "SB_GAM"
      # )
    )
    )
  # browser()
  # tmp <- total_dat %>% 
  #   select(p, method, {{measures}}
  #          # {{measures}} := paste(section, measures, sep=".")
  #   ) #%>% #head
  
    # filter(method %in% c("bglm_spline_de", "blasso_spline",
    #                      "cosso", "acosso", "mgcv", "SB_GAM")) %>% 
    # group_by(p, method) %>% 
    # summarize(
      # across({{measures}}, 
    #          list(mean = mean, sd = sd), na.rm = T,
    #          .names = "{.fn}")#,
    #   # n_miss = sum(is.na(auc))
    # ) %>% 
    # ungroup() %>% 
    # transmute(
    #   p, method,
    #   {{measures}} := sprintf("%.2f (%.2f)", mean, sd)#,
    #   # misclass = sprintf("%.2f (%.2f)", misclass_mean, misclass_sd)
    # ) %>% 
    # pivot_wider(id_cols = "p", names_from = "method",
    #             values_from = {{measures}})
}