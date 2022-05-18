AlphaOptim <- function(alphaHat, alphaTarget, df){
  (mean(df < alphaHat, na.rm = T) - alphaTarget)^2
}

GetEmpAlpha <- function(data, alphaTarget){
  res <- list()
  ### Unfiltered Genes for n == 10
  res$unfiltered$"10" <- sapply(1:9, function(i){                                                      # for all 9 methods
    df <- as.data.frame(data$geneData$unfiltered$"10"[i,])               # Filter our NDe Genes
    df <- df[!permResults$plotData$unfiltered$deMask,]
    optimize(AlphaOptim, interval = c(0,1), alphaTarget = alphaTarget, df = as.matrix(df))[[1]]
  }
  )
  ### Unfiltered Genes for n == 40
  res$unfiltered$"40" <- sapply(1:9, function(i){                                                      # for all 9 methods
    df <- as.data.frame(data$geneData$unfiltered$"40"[i,])               # Filter our NDe Genes
    df <- df[!permResults$plotData$unfiltered$deMask,]
    optimize(AlphaOptim, interval = c(0,1), alphaTarget = alphaTarget, df = as.matrix(df))[[1]]
  }
  )
  ### Filtered Genes for n == 10
  res$filtered$"10" <- sapply(1:9, function(i){                                                      # for all 9 methods
    df <- as.data.frame(data$geneData$filtered$"10"[i,])               # Filter our NDe Genes
    df <- df[!permResults$plotData$unfiltered$deMask,]
    optimize(AlphaOptim, interval = c(0,1), alphaTarget = alphaTarget, df = as.matrix(df))[[1]]
  }
  )
  ###  Filtered Genes for n == 40
  res$filtered$"40" <- sapply(1:9, function(i){                                                      # for all 9 methods
    df <- as.data.frame(data$geneData$filtered$"40"[i,])               # Filter our NDe Genes
    df <- df[!permResults$plotData$unfiltered$deMask,]
    optimize(AlphaOptim, interval = c(0,1), alphaTarget = alphaTarget, df = as.matrix(df))[[1]]
  }
  )
  
  res
}