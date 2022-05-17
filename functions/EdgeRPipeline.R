EdgeRPipeline = function(Y, X, filtering = F, test, ...){
  #Takes X, a table of counts and Y, a factor with two levels.

  design <- model.matrix(~ X)
  dge <- edgeR::DGEList(counts=Y,
                group = design[,2])
  rn <- row.names(Y)

  dge = edgeR::calcNormFactors(dge)
  ### Filtering if selected
  if (filtering){
    keep <- edgeR::filterByExpr(dge,design)
    dge  <- dge[keep,,keep.lib.sizes=FALSE]
    rm(keep)
  }

  # Normalization (TMM followed by voom)
  dge <- edgeR::calcNormFactors(dge)
  dge <- edgeR::estimateDisp(dge)

  if (test == "exact"){
    res <-  edgeR::exactTest(dge)
  } else {
    fit <- edgeR::glmQLFit(dge, design)
    res <- edgeR::glmQLFTest(fit, coef=2)
  }

  
  ### If we only computed p-values for the not-filtered genes, pad the rest w. NAs
  if (filtering){
    df <- data.frame(row.names = rn, pValue = rep(NA, length(rn)))
    df[keep, 1] <- res$tabl$PValue
    
  } else {
    df <- data.frame(row.names = rn, pValue = res$tabl$PValue)
  }
  return(df$pValue)
}
