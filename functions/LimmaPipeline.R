LimmaPipeline = function(Y, X, filtering = F, ...){
  ### Run complete Limma pipeline, using Voom and empirical Bayes shrinkage
  design = model.matrix(~ X)

  dge <- edgeR::DGEList(counts=Y)
  rn  <- row.names(Y)

  dge <- edgeR::calcNormFactors(dge)

  # filtering
  if (filtering){
    keep = edgeR::filterByExpr(dge, design)
    dge  = dge[keep,,keep.lib.sizes=FALSE]
  }


  ### Normalization (TMM followed by voom)
  dge = edgeR::calcNormFactors(dge)
  v   = limma::voom(dge, design, plot=FALSE)

  ### Fit model to data given design
  fit = limma::lmFit(v, design)

  ### extract p-values from moderated t-statistic
  fit = limma::eBayes(fit)

  ### If we only computed p-values for the not-filtered genes, pad the rest w. NAs
  if (filtering){
    df <- data.frame(row.names = rn, pValue = rep(NA, length(rn)))
    df[keep, 1] <- fit$p.value[,2]
    
  } else {
    df <- data.frame(row.names = rn, pValue = fit$p.value[,2])
  }
  return(df$pValue)
}
