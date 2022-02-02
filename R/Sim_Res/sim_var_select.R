sim_var_select_raw <- function(success_rate,
                               dist = "binomial"){
  success_rate %>% 
    filter(dist=={{dist}}, n_success!=0) %>%
    pull(path) %>% 
    map( .f = function(sim){
      sim.df <- unglue_data(sim, 
                            "{}/{}-dis_{dist}-p_{p}") %>% 
        mutate( p = as.numeric(p))
      
      ret <- list.files(sim, full.names = TRUE) %>% 
        grep(".rds", x=., value = TRUE) %>%
        map_dfr(.f = function(.file){
          # Each column vector is the variable seleciton results from each method
          it <- unglue_data(.file, "{whatever}/it_{it}.rds") %>% pull(it) %>% as.numeric
          var_select_res <- read_rds(.file) %>% `[[`("var_sel") %>% 
            rownames_to_column(var = "Variable") %>% 
            mutate(It = it)
          return(var_select_res)
        }) 
      list(p = sim.df$p,
           var_select = ret)
    })
}


sim_comp_select_raw <- function(success_rate,
                                dist = "binomial"){
  success_rate %>% 
    filter(dist=={{dist}}, n_success!=0) %>%
    pull(path) %>% 
    map( .f = function(sim){
      sim.df <- unglue_data(sim, 
                            "{}/{}-dis_{dist}-p_{p}") %>% 
        mutate( p = as.numeric(p))
      
      ret <- list.files(sim, full.names = TRUE) %>% 
        grep(".rds", x=., value = TRUE) %>%
        map_dfr(.f = function(.file){
          # browser()
          it <- unglue_data(.file, "{whatever}/it_{it}.rds") %>% pull(it) %>% as.numeric
          comp_res_raw <- read_rds(.file) %>% `[[`("var_part")
          
          ssGAM_raw <- comp_res_raw$ssGAM %>% unglue_unnest(part, "{Component}({Variable})", remove = FALSE) %>% 
            transmute(It = it, Variable, 
                      Component = case_when(
                        Component == "lin" ~ "Linear",
                        Component == "sm" ~ "Nonlinear",
                        TRUE ~ NA_character_),
                      ssGAM = prob>=0.5)
          bamlasso_raw <- comp_res_raw$bamlasso$`Non-parametric` %>% 
            pivot_longer(cols = ends_with("Linear"), names_to = "bamlasso") %>% 
            transmute(Variable, Component = bamlasso, bamlasso = value )
          
          ssGAM_raw %>% left_join(bamlasso_raw) %>% 
            mutate(bamlasso = replace_na(bamlasso, FALSE)) %>% 
            return()
        }) 
      list(p = sim.df$p,
           comp_select = ret)
    })
}


plot_var_select <- function(var_select, show_null = TRUE){
  # browser()
  
  p <- var_select$p
  
  plot_dat <- var_select$var_select %>% 
    rename(SBGAM = SB_GAM) %>% 
    group_by(Variable) %>%
    summarize(across(lasso:ssGAM, list(mean = mean, sd = sd), na.rm = TRUE)) %>% 
    pivot_longer(cols = lasso_mean:ssGAM_sd, names_to = c("Method", "Measure"), 
                 names_sep = "_", values_drop_na = FALSE) %>% 
    pivot_wider(names_from = Measure, values_from = value)
  
  # browser()

  if(show_null){
    pos_ind <- plot_dat$Variable %in% paste0("x", 1:4)
    pos_vars <- plot_dat[pos_ind, ,drop = FALSE]
    
    null_vars <- plot_dat[!pos_ind, ,drop = FALSE]
    if(nrow(null_vars)==0) plot_dat <- pos_vars
    else {
      tmp <- null_vars %>% group_by(Method) %>% 
        summarize(mean = mean(mean)) %>% 
        mutate(Variable = "x0")
      plot_dat <- rbind(
        pos_vars %>% select(-sd),
        tmp
        )
        
    }
  }
  
  
  ggplot(plot_dat) +
    # geom_jitter(aes(x = Variable, y = mean, color = Method))#+
  geom_bar(aes(x = Variable, y = mean, fill = Method), stat="identity",
           position=position_dodge())
    # geom_errorbar(aes(x = Variable, color = Method, ymin=mean-sd, ymax=mean+sd), width=.1,
    #               # position=position_dodge(.9)
    #               )
  
}

plot_comp_select <- function(comp_select, show_null = TRUE){
  # browser()
  p <- comp_select$p
  
  plot_dat <- comp_select$comp_select %>% 
    # rename(SBGAM = SB_GAM) %>% 
    group_by(Variable, Component) %>%
    summarize(across(c(ssGAM, bamlasso), list(mean = mean, sd = sd), na.rm = TRUE)) %>% 
    pivot_longer(cols = ssGAM_mean:bamlasso_sd, names_to = c("Method", "Measure"), 
                 names_sep = "_", values_drop_na = FALSE) %>% 
    pivot_wider(names_from = Measure, values_from = value)
  
  # browser()
  
  if(show_null){
    pos_ind <- plot_dat$Variable %in% paste0("x", 1:4)
    pos_vars <- plot_dat[pos_ind, ,drop = FALSE]
    
    null_vars <- plot_dat[!pos_ind, ,drop = FALSE]
    if(nrow(null_vars)==0) plot_dat <- pos_vars
    else {
      tmp <- null_vars %>% group_by(Method, Component) %>% 
        summarize(mean = mean(mean)) %>% 
        mutate(Variable = "x0")
      plot_dat <- rbind(
        pos_vars %>% select(-sd),
        tmp
      )
      
    }
  }
  
  plot_dat_fnl <- plot_dat %>% 
    transmute(
      Variable = paste(Variable, Component, sep = "_"),
      Method, mean) 
  
  # browser()
  ggplot( plot_dat_fnl ) +
    # geom_jitter(aes(x = Variable, y = mean, color = Method))#+
    geom_bar(aes(x = Variable, y = mean, fill = Method), stat="identity",
             position=position_dodge()) +
    theme(axis.text.x = element_text(angle = 90))
  # geom_errorbar(aes(x = Variable, color = Method, ymin=mean-sd, ymax=mean+sd), width=.1,
  #               # position=position_dodge(.9)
  #               )
}


var_select_metrics <- function(res, truth){
  stop("Not Implemented")
}



make_sim_var_select_table <- function(success_rate,
                                      dist = "binomial"){
  stop("Not Implemented")
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