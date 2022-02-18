create_success_rate_table <- function(path_list){
  unglue_data(path_list[str_ends(path_list, ".rds")],
              "{path}/it_{}") %>% 
    group_by(path) %>% 
    summarize(n_success = n()) %>% 
    unglue_unnest(
      col = path,
      patterns = "{}/{}-dis_{dist}-p_{p}",
      remove = FALSE
    ) %>% 
    mutate( p = as.numeric(p)) %>% 
    arrange(dist, p) %>% 
    select(dist, p, n_success, path)
}

