GetFDRPlot <- function(data, deMask){
  ### Loop through data w. different sample sizes
  fdr <- lapply(data, function(nDf){
    ### Loop through methods
    fdrDf <- sapply(1:9, function(i){
      
      ### for each method, apply BH for each replication (in columns)
      df <- as.data.frame(nDf[i,])
      df <- apply(df, 2, p.adjust, method = "BH") <= 0.05
      ### Compute FDP
      fdr <- 1 - colSums(df[deMask,], na.rm = T) / colSums(df, na.rm = T)
      names(fdr) <- NULL
      fdr
    })
    ### division by zero --> NA = 0
    fdrDf <- replace_na(fdrDf, 0)
    colnames(fdrDf) <- row.names(data$"12")
    fdrDf
  })
  ### bind results of different sample sizes together, append data for sample size
  fdr <- as.data.frame(rbind(fdr[[1]], fdr[[2]]))
  
  fdr$n <- rep(names(data), each = nrow(fdr)/2)
  
  ### extract label order (for plotting)
  labelOrder <- row.names(data$"12")
  
  ### plot fdr
  fdrPlot <- fdr %>% pivot_longer(cols = 1:9) %>%
    ggplot(mapping = aes(x = name, y = value)) +
    geom_boxplot() +
    facet_wrap(~n) +
    stat_summary(fun=mean, geom="point", shape=20, size=1, color="red", fill="red") +
    geom_hline(yintercept = 0.05, col = "gold") +
    scale_x_discrete(guide = guide_axis(angle = 90), limits = labelOrder) +
    labs(title = "Observed FDP of all Methods \n", y = "FDP", x = "Model Name")
  
  ### Repeat the same steps for the specificity
  nDe <- lapply(data, function(nDf){
    nDeDf <- sapply(1:9, function(i){
      df <- as.data.frame(nDf[i,])
      df <- apply(df, 2, p.adjust, method = "BH") <= 0.05
      nDe <- colSums(df[deMask,], na.rm = T) /
        sum(deMask)
      names(nDe) <- NULL
      nDe
    })
    colnames(nDeDf) <- row.names(data$"12")
    nDeDf
  })
  nDe <- as.data.frame(rbind(nDe[[1]], nDe[[2]]))
  nDe$n <- rep(names(data), each = nrow(nDe)/2)
  nDePlot <- nDe %>% pivot_longer(cols = 1:9) %>%
    ggplot(mapping = aes(x = name, y = value)) +
    geom_boxplot() +
    facet_wrap(~n) +
    scale_x_discrete(guide = guide_axis(angle = 90), limits = labelOrder)+
    labs(title = "Observed Sensitivity \nof all Methods", y = "Sensitivity", x = "Model Name")
  
  pAll <- cowplot::plot_grid(fdrPlot, nDePlot, ncol = 2)
  return(pAll)
}