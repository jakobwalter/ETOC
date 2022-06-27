GetLinePlots <- function(preds, modelnames, alpha = 0.01, DE, title, ymax = 1){
  ##create plots for all parameters
  plot_list <- lapply(names(preds), function(var) {
    df <-preds[[var]] %>%
      tidyr::separate(col = name, into = c("Model", "n", "D"), sep = "_") %>%
      dplyr::filter((Model %in% modelnames) & D == DE)
    
    p1 <- df %>%
      ggplot(mapping = aes(x = x, y= value, col = Model)) +
      geom_line() +
      labs(x = var, y = "P(p <= 0.01)") +
      facet_wrap(~n) +
      scale_y_continuous(breaks = c(0, 0.01, 0.05, 0.1, 0.2, 0.5, 1), limits = c(0, ymax), trans = "sqrt", expand=c(0,0))
    
    if(DE == "NDE"){
      p1 + geom_hline(yintercept = alpha)
    } else{
      p1
    }
  })
  plot_list[[1]] <- plot_list[[1]] + scale_x_continuous(trans = "sqrt", limits = c(0, 2900), breaks = c(0, 500, 1000, 2000, 3000), expand=c(0,0))
  plot_list[[2]] <- plot_list[[2]] + scale_x_continuous(trans = "sqrt", limits = c(0, 4.5), breaks = c(0, 1, 2, 4), expand=c(0,0))
  
  plot_list[[1]] <- plot_list[[1]] + labs(title = title) +
    theme(legend.position="none")
  plot_list[[3]] <- plot_list[[3]] + theme(legend.position="none")
  p_all <- cowplot::plot_grid(plot_list[[1]], plot_list[[2]], plot_list[[3]], nrow = 3,
                              align = "v", axis = "r")
  
  return(p_all)
}