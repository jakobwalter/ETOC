GetLinePlots <- function(preds, modelnames, DE, title, ymax = 1){
  plot_list <- lapply(names(preds), function(var) {
    df <-preds[[var]] %>%
      tidyr::separate(col = name, into = c("Model", "n", "D"), sep = "_") %>%
      dplyr::filter((Model %in% modelnames) & D == DE)
    
    df %>%
      ggplot(mapping = aes(x = x, y= value, col = Model)) +
      geom_line() +
      geom_hline(yintercept = 0.05) +
      labs(x = var, y = "P(p <= 0.01)") +
      facet_wrap(~n) +
      scale_y_log10() +
      scale_y_continuous(breaks= c(0, 0.05, 0.1, 0.2, 0.5, 1), limits = c(0, ymax))
  })
  
  plot_list[[1]] <- plot_list[[1]] + labs(title = title) +
    theme(legend.position="none")
  plot_list[[3]] <- plot_list[[3]] + theme(legend.position="none")
  p_all <- cowplot::plot_grid(plot_list[[1]], plot_list[[2]], plot_list[[3]], nrow = 3,
                              align = "v", axis = "r")
  
  return(p_all)
}