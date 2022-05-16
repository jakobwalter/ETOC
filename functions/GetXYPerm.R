GetXYperm <- function(n, XDE, XNDE, Y, DEInd, normFactors){

  perm <- sample(1:ncol(XNDE))

  #get permutation accounting for library sizes

  ##first we subset the NDE genes, then we "normalize" them, apply the permutation
  ##and then undo the normalization again
  XNDEPerm <- (XNDE * normFactors)[,perm] /normFactors

  colnames(XNDE) <- NULL
  colnames(XDE) <- NULL
  XPerm <- rbind(XDE, XNDEPerm)


  #subset random samples
  g1 <- sample(which(Y == F), n/2)
  g2 <- sample(which(Y == T), n/2)


  XPerm <- XPerm[,c(g1,g2)]
  YPerm <- Y[c(g1,g2)]

  XPerm <- round(XPerm)

  mode(XPerm) <- "integer"

  return(list("X" = XPerm, "Y" = YPerm))
}
