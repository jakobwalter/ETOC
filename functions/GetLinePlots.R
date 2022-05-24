GetLinePlots <- function(preds, modelnames, alpha = 0.01, DE, title, ymax = 1){
  plot_list <- lapply(names(preds), function(var) {
    df <-preds[[var]] %>%
      tidyr::separate(col = name, into = c("Model", "n", "D"), sep = "_") %>%
      dplyr::filter((Model %in% modelnames) & D == DE)
    
    p1 <- df %>%
      ggplot(mapping = aes(x = x, y= value, col = Model)) +
      geom_line() +
      labs(x = var, y = "P(p <= 0.01)") +
      facet_wrap(~n) +
      scale_y_continuous(limits = c(0, ymax))
    
    if(DE == "NDE"){
      p1 + geom_hline(yintercept = alpha)
    } else{
      p1
    }
  })
  
  plot_list[[1]] <- plot_list[[1]] + labs(title = title) +
    theme(legend.position="none")
  plot_list[[3]] <- plot_list[[3]] + theme(legend.position="none")
  p_all <- cowplot::plot_grid(plot_list[[1]], plot_list[[2]], plot_list[[3]], nrow = 3,
                              align = "v", axis = "r")
  
  return(p_all)
}