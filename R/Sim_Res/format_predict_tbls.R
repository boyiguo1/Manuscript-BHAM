format_predict_tbls <- function( dat, caption, label){
  dat %>% 
    mutate(p = as.integer(p),
           mgcv = replace( mgcv, p %in% c(100,200) , "-")) %>% 
    select(
      "P" = p,
      "mgcv" = mgcv,
      "LASSO" = lasso,
      "COSSO" = cosso,
      "Adaptive COSSO" = acosso,
      # "BHAM-IWLS" = bglm_spline_de,
      # "BHAM-CD" = bamlasso,
      "BHAM" = bamlasso,      
      "SB-GAM" = SB_GAM,
      "spikeSlabGAM" = ssGAM
    ) %>% 
    xtable(
      align = "ccccccccc",
      caption = caption,
      label = label ) %>% 
    print(comment = FALSE, include.rownames = FALSE)
}