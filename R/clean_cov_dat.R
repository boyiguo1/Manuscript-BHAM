#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

clean_cov_dat <- function(dat) {

  # browser()
  
  ret <- dat %>% 
    mutate(across(c(HOMA_PC, Triglycerides_Baseline,
                    Weight_PC), as.numeric)) %>% 
    remove_empty() %>% 
    remove_constant() %>% 
    rename(out_HOMA_PC = HOMA_PC) %>% 
    rename_with(.fn = ~paste0("cov_", .x),
                .cols = c(Age, Sex, Race, Triglycerides_Baseline,
                          Weight_PC))
  
  # # Table of Percentage Missing
  # miss_pc <- ret %>% 
  #   select(-Sample_ID) %>%
  #   mutate(across(everything(),
  #                 is.na)) %>% 
  #   summarize(across(everything(),mean)) %>% 
  #   pivot_longer(cols = everything(), names_to = "var")
  
  return(ret)

}
