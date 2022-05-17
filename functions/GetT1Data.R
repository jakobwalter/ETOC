GetT1Data <- function(PlotData, GeneData, alphas){
  t1_data <- lapply(GeneData, function(i){
    sapply(1:nrow(i), function(j){
      df  <- as.data.frame(i[j,]) <= alphas[j]
      df <- replace_na(df, F)
      rowMeans(df, na.rm = T)
    })
  })

  t1_data <- lapply(t1_data, as.data.frame)


  t1_data <- lapply(t1_data, function(df){
    df <- df
    colnames(df) <- row.names(GeneData$"10")
    cbind(df, PlotData)
  })

  #bind rows together
  t1_data <- dplyr::bind_rows(t1_data)
  t1_data$n <- rep(names(GeneData), each = nrow(PlotData))

  t1_data <- tidyr::pivot_longer(t1_data, cols = 1:nrow(GeneData$"10"))

  #t1_data$name <-

  return(t1_data)
}
