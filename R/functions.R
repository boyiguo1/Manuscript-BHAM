#' Use GAM to screen the features
#'
#' @param dat the raw data
#' @param nrow How many rows are selected
#'
#' @return
#'
gam_screen <- function(dat, nrow = 200){
  dat %>% 
    select(starts_with("mz")) %>% 
    names() %>% 
    map_dfr(#feature_name[1:2],
      .f = function(name, .dat){
        # name <- feature_name[1]
        y <- .dat %>% pull(death3yr)
        x <- .dat %>% pull({{name}})
        mgcv::gam(y ~ s(x, bs = "cr", k = 10), family = binomial(),
                  data = data.frame(x, y)) %>% 
          tidy() %>% 
          filter(term == "s(x)") %>% 
          select(p.value) %>% 
          mutate(var = name,
                 p.value)
        
      },
      .dat = dat) %>% data.frame(
        .,
        p.adj = p.adjust(.$p.value, "fdr")
      ) %>% arrange(p.adj) %>% #filter(p.adj<0.2)
    head(nrow)
}


create_success_rate_table <- function(path_list){

  # sims <- list.files(path,full.names =TRUE)
  # browser()
  unglue_data(path_list[str_ends(path_list, ".rds")],
              "{path}/it_{}") %>% 
    group_by(path) %>% 
    summarize(n_success = n()) %>% 
    unglue_unnest(
      col = path,
      patterns = "{}/bgam_{study}_-dis_{dist}-p_{p}",
      remove = FALSE
    ) %>% 
    mutate( p = as.numeric(p)) %>% 
    arrange(dist, p) %>% 
    select(dist, p, n_success, path)
  #   head()
  # 
  # 
  # success_rate <- map_dfr(path_list, 
  #                         .f = function(sim_file){
  #                           browser()
  #   sim.df <- unglue_data(sim_file, 
  #                         "{}/bgam_{study}_-dis_{dist}-p_{p}/{}", 
  #                         kee) %>% 
  #     mutate( p = as.numeric(p)) %>% 
  #     group_by(dist)
  #     summarize()
  #   fls <- list.files(sim_file, full.names = TRUE) 
  #   fls <- fls[grep(".rds", x=fls)]
  #   n <- length(fls)
  #   
  #   data.frame(sim.df, n_success = n, path = sim)
  # }) %>% 
  #   arrange(p) 

  # return(success_rate)  
}


make_sim_main_plots <- function(success_rate, dist, measures){
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
        # arrange(mdl, s0) %>% 
        mutate(#s0 = factor(s0),
          p = sim.df$p)#  %>% 
      
    }) %>% 
    mutate(method = factor(method, levels = c("bglm_t", "bglm_de", "blasso",
                                              "bglm_spline_t", "bglm_spline_de", "blasso_spline",
                                              "cosso", "acosso", "mgcv"
    )))
  
  
  measures %>%
    map(.f = function(.name){
      total_dat %>% 
        select(p, method, starts_with("test.")) %>% #head
        pivot_longer(cols = starts_with("test.")) %>% 
        filter(name == {{.name}}) %>%
        # filter(name == {{measures}}) %>%
        ggplot(#data = binom_total_dat,
          aes(x = method, y = value)) +
        geom_jitter() +
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = 45))+
        # coord_flip() +
        facet_grid(cols = vars(p),
                   rows= vars(name)#,
                   # scale = "free_y"
                   )
    }) %>%
  ggarrange(plotlist = ., ncol = 1)
}

