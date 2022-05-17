FlipScoresPipeline = function(Y, X, family,
                              scoreType,
                              nFlips,
                              filtering = F,
                              ...){

  rn <- row.names(X)

  #compute Offset
  d <- edgeR::DGEList(counts = Y)
  d <- edgeR::calcNormFactors(d)
  os <- edgeR::getOffset(d)

  # filtering
  if (filtering){
    keep = edgeR::filterByExpr(d)
    d <- d[keep,]
    d <- edgeR::calcNormFactors(d)
    os <- edgeR::getOffset(d)
  }

  X <- (X == X[1])*2-1

  Y <- d$counts
  #os <- log(colMeans(X))

  #loop through columns and apply flipscores test to each
  pVals <- sapply(1:nrow(Y), function(i) {

    Yi <- Y[i,] #needed to avoid error in formula
    tryCatch(expr = {
      if(family == "negbinom"){
        mod0 <- flipscores::flipscores(Yi ~ X + offset(os),
                           family = "negbinom",
                           score_type = scoreType,
                           n_flips = n_flips, to_be_tested = 2)
      }
      else if(family == "poisson"){
        mod0 <- flipscores::flipscores(Yi ~ X + offset(os),
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
      print(Yi)
      return(NA)
    }
    )
  })

  ### If we only computed p-values for the not-filtered genes, pad the rest w. NAs
  if (filtering){
    df <- data.frame(row.names = rn, pValue = rep(NA, length(rn)))
    df[keep, 1] <- pVals
  } else {
    df <- data.frame(row.names = rn, pValue = pVals)
  }
  return(df$pValue)
}
