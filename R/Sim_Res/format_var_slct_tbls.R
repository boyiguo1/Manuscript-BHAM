format_var_slct_tbls <- function( dat, caption, label){
  dat %>% 
    mutate(Metric = factor(Metric, 
                           levels = c("precesion", "recall", "mcc"),
                           labels = c("Precision", "Recall", "MCC")),
           p = as.integer(p)
    )%>% 
    filter(!(Metric == "MCC" & p==4)) %>%
    arrange(Metric, p) %>% 
    select(
      "P" = p,
      Metric,
      # "mgcv" = mgcv,
      "LASSO" = lasso,
      "COSSO" = cosso,
      "Adaptive COSSO" = acosso,
      # "BHAM-IWLS" = bglm_spline_de,
      # "BHAM-CD" = bamlasso,
      "BHAM" = bamlasso,      
      "SB-GAM" = SBGAM,
      "spikeSlabGAM" = ssGAM
    ) %>% 
    xtable(
      align = "ccccccccc",
      caption = caption,
      label = label
    ) %>% 
    print(comment = FALSE, include.rownames = FALSE,
          hline.after = c(-1, 0, nrow(.), 5,10))
}