LimmaPipeline = function(X, Y, filtering = F, ...){

  design = model.matrix(~ Y)

  dge = edgeR::DGEList(counts=X)

  dge = edgeR::calcNormFactors(dge)

  # filtering
  if (filtering){
    keep = edgeR::filterByExpr(dge, design)
    dge  = dge[keep,,keep.lib.sizes=FALSE]
  }


  # Normalization (TMM followed by voom)
  dge = edgeR::calcNormFactors(dge)
  v   = limma::voom(dge, design, plot=FALSE)

  # Fit model to data given design
  fit = limma::lmFit(v, design)

  #extract p-values from moderated t-statistic
  fit = limma::eBayes(fit)

  pVals <- pVals <- data.frame("pValue" = fit$p.value[,2])

  df <- data.frame(row.names = rownames(X), pValue = rep(NA, nrow(X)))

  df[rownames(df) %in% rownames(pVals), 1] <- pVals$pValue
  return(df)
}
