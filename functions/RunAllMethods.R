### Helper Function to run all Methods
RunAllMethods <- function(Y, X, nFlipsDP = 2000, nFlipsFS = 400){
  suppressMessages(suppressWarnings({
    res <- list()
    res$Limma    <- LimmaPipeline(Y, X)
    res$"EdgeR-Et"  <- EdgeRPipeline(Y, X, test = "exact")
    res$"EdgeR-Ql"  <- EdgeRPipeline(Y, X, test = "ql")
    res$DESeq2   <- DESeq2Pipeline(Y, X)
    res$MWU      <- MWUPipeline(Y, X)
    res$BP       <- DirectPermutationsPipeline(X, Y, nFlips = nFlipsDP)
    res$FsPB     <- FlipScoresPipeline(Y, X, "poisson", scoreType = "basic", nFlips = nFlipsFS)
    res$FsPE     <- FlipScoresPipeline(Y, X, "poisson", scoreType = "effective", nFlips = nFlipsFS)
    res$FsPS     <- FlipScoresPipeline(Y, X, "poisson", scoreType = "standardized", nFlips = nFlipsFS)
  }))
  return(res)
}