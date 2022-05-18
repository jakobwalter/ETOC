### Helper Function to run all Methods
RunAllMethods <- function(Y, X){
  suppressMessages(suppressWarnings({
    res <- list()
    res$Limma    <- LimmaPipeline(Y, X)
    res$EdgeREt  <- EdgeRPipeline(Y, X, test = "exact")
    res$EdgeRQl  <- EdgeRPipeline(Y, X, test = "ql")
    res$DESeq2   <- DESeq2Pipeline(Y, X)
    res$MWU      <- MWUPipeline(Y, X)
    res$DP       <- DirectPermutationsPipeline(X, Y, nFlips = 1000)
    res$FsPB     <- FlipScoresPipeline(Y, X, "poisson", scoreType = "basic", nFlips = 100)
    res$FsPE     <- FlipScoresPipeline(Y, X, "poisson", scoreType = "effective", nFlips = 100)
    res$FsPS     <- FlipScoresPipeline(Y, X, "poisson", scoreType = "standardized", nFlips = 100)
  }))
  return(res)
}