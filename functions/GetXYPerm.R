GetXYperm <- function(n, YDe, YNDe, X, DeInd, normFactors){

  ### get permutation accounting for library sizes
  ### first we subset the NDE genes, then we "normalize" them, apply the permutation
  ### and then undo the normalization again
  perm <- sample(1:ncol(YNDe))
  YNDePerm <- (YNDe * normFactors)[,perm] /normFactors

  ### Remove Column Names and Bind genes together
  YPerm <- rbind(YDe, YNDePerm)
  colnames(YPerm) <- NULL

  ### subset random samples
  g1 <- sample(which(X == F), n/2)
  g2 <- sample(which(X == T), n/2)
  YPerm <- YPerm[,c(g1,g2)]
  XPerm <- X[c(g1,g2)]

  ### Round to integers (in case of computational errors during normalization)
  YPerm <- round(YPerm)
  mode(YPerm) <- "integer"

  return(list("X" = XPerm, "Y" = YPerm))
}
