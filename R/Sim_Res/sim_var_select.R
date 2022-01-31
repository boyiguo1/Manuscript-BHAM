sim_var_select_raw <- function(success_rate,
                               dist = "binomial"){
  stop("not implmented")
}


sim_comp_select_raw <- function(success_rate,
                               dist = "binomial"){
  stop("not implmented")
}


plot_var_select <- function(var_select){
  stop("not implmented")
}

plot_comp_select <- function(comp_select){
stop("not implmented")
}


make_sim_var_select_table <- function(success_rate,
                                      dist = "binomial"){
  # browser()
  total_dat <- success_rate %>% 
    filter(dist=={{dist}}, n_success!=0) %>%
    pull(path) %>% 
    map_dfr( .f = function(sim){
      sim.df <- unglue_data(sim, 
                            "{}/{}-dis_{dist}-p_{p}") %>% 
        mutate( p = as.numeric(p))
      
      list.files(sim, full.names = TRUE) %>% 
        grep(".rds", x=., value = TRUE) %>%
        map_dfr(.f = function(.file){
          browser()
          it <- unglue_data(.file, "{whatever}/it_{it}.rds") %>% pull(it) %>% as.numeric
          
          # Each column vector is the variable seleciton results from each method
          var_select_res <- read_rds(.file) %>% `[[`("var_sel")   
          truth_vec <- rep(FALSE, nrow(var_select_res)) 
          truth_vec[1:4] <- TRUE
          
          # TODO (boyiguo1): calculate variable selection metrics, via a wrapper funciton
          
          # TODO (boyiguo1): Column wise apply to the var_select_res
        
          # TODO (boyiguo1): return a 3*n matrix where the first column is method, second column is measure, and the third column is the value
          
          comp_res <- read_rds(.file) %>% `[[`("var_part")
          # TODO (boyiguo1): summarize ssGAM and bamlasso performance.
          
          # TODO (boyiguo1): Extract the var_part results, by creating a var_slect_res vector by parts (lnr and nonlinr)
          
          # TODO(boyiguo1): construct 
          
          
          
          
          ret <- data.frame(
            it = it,
            # TODO (boyiguo1): Insert each method results.
            
          )
        }) %>%
        arrange(it)
    }) 
  
  # TODO(boyiguo1): organize the data for an output format
  # total_dat %>% 
  #   select(p, method, {{measures}}
  #          # {{measures}} := paste(section, measures, sep=".")
  #   ) %>% #head
  #   group_by(p, method) %>% 
  #   summarize(
  #     across({{measures}}, 
  #            list(mean = mean, sd = sd), na.rm = T,
  #            .names = "{.fn}")
  #   ) %>% 
  #   ungroup() %>% 
  #   transmute(
  #     p, method,
  #     {{measures}} := sprintf("%.2f (%.2f)", mean, sd)#,
  #     # misclass = sprintf("%.2f (%.2f)", misclass_mean, misclass_sd)
  #   ) %>% 
  #   pivot_wider(id_cols = "p", names_from = "method",
  #               values_from = {{measures}})
}