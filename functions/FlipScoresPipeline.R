FlipScoresPipeline = function(X, Y, family,
                              scoreType,
                              nFlips,
                              verbose = FALSE,
                              filtering = F,
                              ...){

  rn <- row.names(X)

  #compute Offset
  d <- edgeR::DGEList(counts = X)
  d <- edgeR::calcNormFactors(d)
  os <- edgeR::getOffset(d)

  # filtering
  if (filtering){
    keep = edgeR::filterByExpr(d)
    d <- d[keep,]
    d <- edgeR::calcNormFactors(d)
    os <- edgeR::getOffset(d)
  }


  Y <- (Y == Y[1])*2-1


  X <- d$counts
  #os <- log(colMeans(X))

  #loop through columns and apply flipscores test to each
  pVals <- sapply(1:nrow(X), function(i) {

    #Optionally print progress
    if (verbose){
      if(i %% 50 == 0){
        cat("\r", round(i/nrow(X), 3))
      }
    }

    Xi <- X[i,] #needed to avoid error in formula
    tryCatch(expr = {
      if(family == "negbinom"){
        mod0 <- flipscores::flipscores(Xi ~ Y + offset(os),
                           family = "negbinom",
                           score_type = scoreType,
                           n_flips = n_flips, to_be_tested = 2)
      }
      else if(family == "poisson"){
        mod0 <- flipscores::flipscores(Xi ~ Y + offset(os),
                           family = poisson,
                           score_type = scoreType,
                           n_flips = nFlips, to_be_tested = 2)
      }

      #extract p-value from summary

      coef(summary(mod0))[2, 5]
    },
    error = function(e) {
      cat("\r","Error in Gene ", i)
      print(e)
      print(X_i)
      return(NA)
    }
    )
  })

  if (filtering){
    df <- data.frame(row.names = rn, p.value = rep(NA, length(rn)))
    df[keep, 1] <- pVals
  } else {
    df <- data.frame(row.names = rn, p.value = pVals)
  }

  return(df)
}
