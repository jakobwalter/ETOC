MWUPipeline = function(Y, X, roundDigits = 0, filtering = F, ...){

  ### Created Mask from Group Labels and initiate dge object
  g1    <- X == X[1]
  rn    <- row.names(Y)
  dge    <- edgeR::DGEList(counts=Y)

  ### filtering
  if (filtering){
    keep = edgeR::filterByExpr(dge)
    dge  = dge[keep,,keep.lib.sizes=FALSE]

  }

  ### Compute counts per million normalized by library sizes
  dge <- edgeR::calcNormFactors(dge, method="TMM")
  dgeCpm <- edgeR::cpm(dge)

  ### Apply Wilcoxon Test to All Genes using mask
  pVals <- sapply(1:nrow(dgeCpm), function(i) {
    wilcox.test(dgeCpm[i, g1], dgeCpm[i, !g1])$p.value
  })

  ### If we only computed p-values for the not-filtered genes, pad the rest w. NAs
  if (filtering){
    df <- data.frame(row.names = rn, pValue = rep(NA, length(rn)))
    df[keep, 1] <- pVals
    pVals <- df$pValue
    
  } else {
    df <- data.frame(row.names = rn, pValue = pVals)
  }
  return(df$pValue)
}
