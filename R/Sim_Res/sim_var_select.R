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


# var_select_metrics <- function(res, truth){
#   tibble(
#     recall = recall_vec(truth = truth, estimate = res),
#     precsion = precision_vec( truth = truth, estimate = res),
#     # ppv(dat, truth = truth, estimate = bamlasso),
#     mcc = mcc_vec(truth = truth, estimate = res)
#   )
# }



make_sim_var_metric_raw <- function(var_select){
  # browser()
  p <- var_select$p
  
  # TODO (boyiguo1): check if some method, return NA for variable selection, and how does that affect constructing the iteration level var_select metrics
  var_select$var_select %>% 
    rename(SBGAM = SB_GAM) %>% 
    # mutate(across(bamlasso:ssGAM, factor))
    mutate(
      # TODO: check if truth is correct when p > 4
      truth = case_when(
        Variable %in% paste0("x", 1:4) ~ TRUE,
        TRUE ~ FALSE),
      across(c(lasso:ssGAM, truth), factor, levels=c(TRUE, FALSE), labels = c("Positive", "Negative"))) %>% 
    group_split(It, .keep=TRUE) %>% # Doesn't matter if .keep = TRUE/FALSE
    imap_dfr(.f = function(dat, it){
      # browser()
      # If need to use a wrapper function to calculate variable selection metrics, see var_select_metrics
      # Currently use across funciton to calculate the metrics
      dat %>% 
        summarize(
          across(lasso:ssGAM, 
                 list(recall = recall_vec,
                      precesion = precision_vec,
                      mcc = mcc_vec), 
                 truth=.$truth)) %>% 
        pivot_longer(cols = lasso_recall:ssGAM_mcc,
                     names_to = c("Method", "Metric"),
                     names_sep = "_") %>% 
        mutate(It = it)
    })
  
  
  
  
  
  
  # TODO (boyiguo1): return a 3*n matrix where the first column is method, second column is measure, and the third column is the value
  
  
  
  
  
  
  
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