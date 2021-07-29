#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dat
clean_meta_dat <- function(dat, missing_pc = 0.1) {

  
  ret <- dat %>% 
    dplyr::rename(
         Sample_ID = `...1`) %>% 
    mutate(across(-Sample_ID, as.numeric)) %>% 
    janitor::remove_empty() %>% 
    janitor::remove_constant()

  # Table of Percentage Missing
  miss_pc <- ret %>% 
    select(-Sample_ID) %>%
    mutate(across(everything(),
                  is.na)) %>% 
    summarize(across(everything(),mean)) %>% 
    pivot_longer(cols = everything(), names_to = "var")
  
  meta_index <- miss_pc %>% 
    filter(value < missing_pc) %>% 
    # nrow() %>% 
    pull(var)
  # browser()  
  # ret <- 
    ret %>% 
    select(Sample_ID, all_of(meta_index)) %>% 
    rename_with(
             ~paste0("mb_", .x),
             .cols = dplyr::all_of(meta_index))
  
}
