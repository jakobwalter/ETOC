EdgeRPipeline = function(X, Y, filtering = F, test, ...){
  #Takes X, a table of counts and Y, a factor with two levels.

  design = model.matrix(~ Y)

  dge = edgeR::DGEList(counts=X,
                group = design[,2])

  dge = edgeR::calcNormFactors(dge)
  # filtering
  if (filtering){
    keep = edgeR::filterByExpr(dge,design)
    dge = dge[keep,,keep.lib.sizes=FALSE]
    rm(keep)
  }


  # Normalization (TMM followed by voom)
  dge = edgeR::calcNormFactors(dge)

  dge <- edgeR::estimateDisp(dge)

  if (test == "exact"){
    res <-  edgeR::exactTest(dge)
  } else {
    fit <- edgeR::glmQLFit(dge, design)
    res <- edgeR::glmQLFTest(fit, coef=2)
  }


  #pad filtered out genes with NA
  pVals <- res$table %>% dplyr::select(PValue)

  df <- data.frame(row.names = row.names(X), pValue = rep(NA, nrow(X)))

  df[rownames(df) %in% rownames(pVals), 1] <- pVals$PValue
  return(df)
}
