DESeq2Pipeline = function(Y, X, filtering = F, verbose = F, ...){
  rn <- row.names(Y)
  #estimation of overdispersion
  X   <- data.frame("Treatment" = as.factor(X))
  dds <- DESeq2::DESeqDataSetFromMatrix(Y,
                                        colData = X,
                                        design = ~ Treatment)
  dds <- DESeq2::DESeq(dds, quiet = verbose)
  


  topGenes <- as.data.frame(DESeq2::results(dds, alpha = 1-1e-15, pAdjustMethod = "none",
                                            independentFiltering = filtering))
  
  ### If we only computed p-values for the not-filtered genes, pad the rest w. NAs
  if (filtering){
    df <- data.frame(row.names = rn, pValue = rep(NA, length(rn)))
    df[keep, 1] <- topGenes$pvalue
    
  } else {
    df <- data.frame(row.names = rn, pValue = topGenes$pvalue)
  }
  return(df$pValue)
}
