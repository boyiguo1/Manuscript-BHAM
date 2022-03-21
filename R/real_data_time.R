create_real_data_time_tbl <- function(...){
  

  
  # Real Work
  time_tbl_raw <- tar_meta() %>% 
    select(name, seconds) %>%
    filter(name %in% c("ECB_bamlasso_cv", "ECB_bamlasso_fnl", "ECB_SBGAM_cv_raw", "ECB_SBGAM_fnl",
                       "WLM_bamlasso_cv","WLM_bamlasso_fnl", "WLM_SBGAM_cv_raw", "WLM_SBGAM_fnl")) %>% 
    unglue::unglue_unnest(name,
                          "{data}_{model}_{step}", remove = FALSE) %>% 
    arrange(data, model, step) %>%
    pivot_wider(id_cols = data,
                names_from = c("model", "step"),
                values_from = "seconds") %>% 
    rename(SBGAM_cv = SBGAM_cv_raw) %>% 
    transmute(
      data, bamlasso_cv, bamlasso_fnl, 
      bamlasso_total = bamlasso_cv + bamlasso_fnl,
      SBGAM_cv, SBGAM_fnl,
      SBGAM_total = SBGAM_cv + SBGAM_fnl) 
  
  time_tbl_header <- time_tbl_raw %>% 
    colnames %>% data.frame(col_keys = .) %>% 
    unglue::unglue_unnest(
      col_keys,"{line2}_{line3}",
      na = "Data",
      remove = FALSE) %>% 
    mutate(line2 = recode(line2, bamlasso = "BHAM",
                          SBGAM = "SB-GAM"),
           line3 = recode(line3, cv = "CV", fnl = "Final",
                          total = "Total"))
  
  flextable(time_tbl_raw) %>% 
    colformat_double(digits = 1) %>% 
    set_header_df(mapping = time_tbl_header) %>% 
    set_header_labels(bamlasso = "BHAM",
                      SBGAM = "SB-GAM",
                      cv = "CV",
                      fnl = "Final",
                      total = "Total") %>% 
    merge_h(part = "header", i = 1) %>% 
    merge_v(part = "header", j = 1) %>% 
    flextable::theme_booktabs() %>% 
    flextable::align(align = "center", part = "all")
  
}