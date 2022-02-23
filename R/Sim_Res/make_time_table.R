make_time_table <- function(success_rate){
  total_dat <- success_rate %>% 
    filter(n_success!=0) %>% 
    pull(path) %>% 
    map_dfr( .f = function(sim){
      
      
      sim.df <- unglue_data(sim, 
                            "{}/{}-dis_{dist}-p_{p}") %>% 
        mutate( p = as.numeric(p))
      fls <- list.files(sim, full.names = TRUE) 
      fls <- fls[grep(".rds", x=fls)]
      list.files(sim, full.names = TRUE) %>%
        grep(".rds", x=., value = TRUE) %>%
        map_dfr(.f = function(.file){
          it <- unglue_data(.file, "{whatever}/it_{it}.rds") %>% pull(it) %>% as.numeric
          ret <- data.frame(
            it = it,
            read_rds(.file) %>% 
              `[[`("time")%>% 
              data.frame() %>% 
              rownames_to_column("method")
          )
        }) %>%
        arrange(it) %>% 
        mutate(p = sim.df$p,
               dist = sim.df$dist)
      
    }) %>% 
    select(dist, p, it, method, elapsed) %>%
    pivot_wider(names_from = method, values_from = elapsed) %>% 
    transmute(dist = case_when(dist=="binomial"~"Binomial",
                               dist=="gaussian"~"Gaussian"),
              p = as.integer(p), it, mgcv, cosso, acosso,
              `BHAM` = bamlasso_cv + bamlasso_final,
              # `BHAM-IWLS` = bglm_spline_de_cv + bglm_spline_de_final,
              `SB-GAM` = bai_cv + bai_final,
              `spikeSlabGAM` = ssGAM) %>% 
    group_by(dist, p) %>%  
    summarize(
      across(mgcv:`spikeSlabGAM`, ~sprintf("%.2f (%.2f)", mean(.x), sd(.x))
      )
    ) %>% 
    ungroup()
  
  total_dat[total_dat$p %in% c(100,200), "mgcv"] <- "-"
  total_dat
  
  # 
  # 
  # 
  # 
  # 
  # 
  # %>% 
  #   mutate(method = factor(
  #     method,
  #     levels = c("bglm_t", "bglm_de", "blasso",
  #                "bglm_t_group", "bglm_de_group", "blasso_group",
  #                "bglm_spline_t", "bglm_spline_de", "blasso_spline",
  #                "cosso", "acosso", "mgcv", "SB_GAM"
  #     )
  #   )
  #   )
}