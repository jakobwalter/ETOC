DESeq2Pipeline = function(X, Y, filtering = F, verbose = F, ...){
  #estimation of overdispersion
  Y   <- data.frame("Treatment" = as.factor(Y))
  dds <- DESeq2::DESeqDataSetFromMatrix(X,
                                colData = Y,
                                design = ~ Treatment)
  dds <- DESeq2::DESeq(dds, quiet = verbose)


  topGenes <- as.data.frame(DESeq2::results(dds, alpha = 1-1e-15, pAdjustMethod = "none",
                                            independentFiltering = filtering))

  df <- data.frame(row.names = rownames(X), pValue = rep(NA, nrow(X)))

  df[rownames(df) %in% rownames(topGenes), 1] <- topGenes$pvalue

  return(df)
}
