DirectPermutationsPipeline <- function(Y, X, nFlips, filtering = F, verbose = F){

  rn <- row.names(X)

  #compute Offset
  d <- edgeR::DGEList(counts = X)
  d <- edgeR::calcNormFactors(d)
  os <- edgeR::getOffset(d)

  # filtering
  if (filtering){
    keep = edgeR::filterByExpr(d)
    d <- d[keep,]
  }

  X <- d$counts * d$samples$norm.factors

  Y <- Y == Y[1]

  pVals <- sapply(1:nrow(X), function(i){
    #Optionally print progress
    if (verbose){
      if(i %% 50 == 0){
        cat("\r", round(i/nrow(X), 3))
      }
    }

    meanDiffs    <- numeric(nFlips)
    meanDiffs[1] <- abs(mean(X[i,Y == T]) - mean(X[i,Y == F]))
    for (j in 2:nFlips){
      yj <- sample(Y)
      meanDiffs[j] <- abs(mean(X[i,yj == T]) - mean(X[i,yj  == F]))
    }
    pvPerm <- (sum(meanDiffs[1] <= meanDiffs))/nFlips
    return(pvPerm)
  })

  if (filtering){
    df <- data.frame(row.names = rn, pValue = rep(NA, length(rn)))
    df[keep, 1] <- pVals
  } else {
    df <- data.frame(row.names = rn, pValue = pVals)
  }

  return(df$pValue)
}






