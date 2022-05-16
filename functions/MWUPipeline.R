MWUPipeline = function(X, Y, roundDigits = 0, filtering = F, ...){
  #Takes X, a table of counts and Y, a factor with two levels.
  #Returns  unsorted topTable

  g1 <- Y== Y[1]
  rn <- row.names(X)


  design = model.matrix(~ Y)

  dge = edgeR::DGEList(counts=X)


  # filtering
  if (filtering){
    keep = edgeR::filterByExpr(dge, design)
    dge  = dge[keep,,keep.lib.sizes=FALSE]

  }


  dge <- edgeR::calcNormFactors(dge, method="TMM")

  #compute counts per million normalized by library sizes
  dgeCpm <- round(edgeR::cpm(dge), roundDigits)

  # Apply Wilcoxon Test to All Genes
  pVals <- sapply(1:nrow(dgeCpm), function(i) {
    wilcox.test(dgeCpm[i, g1], dgeCpm[i, !g1])$p.value
  })


  if (filtering){
    df <- data.frame(row.names = rn, pValue = rep(NA, length(rn)))
    df[keep, 1] <- p_vals
  } else {
    df <- data.frame(row.names = rn, p.value = pVals)
  }

  return(df)
}
