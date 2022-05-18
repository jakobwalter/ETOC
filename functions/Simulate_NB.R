SimulateNB <- function(nSamples,
                       nGenes,
                       offset = rep(1, nSamples),
                       mu_1,
                       mu_2,
                       phi_1,
                       phi_2){


  ## We create the matrices of the two groups seperately. Each Matrix
  ## must consist of n_genes rows and n_samples/2 columns.
  Y_1 <- rnbinom(nGenes*nSamples/2, mu = rep(mu_1, nSamples/2), size = rep(1/phi_1, nSamples/2))
  Y_1 <- matrix(Y_1, nrow = nGenes)

  Y_2 <- rnbinom(nGenes*nSamples/2, mu = rep(mu_2, nSamples/2), size = rep(1/phi_2, nSamples/2))
  Y_2 <- matrix(Y_2, nrow = nGenes)

  Y_all <- cbind(Y_1, Y_2) #+ exp(offset)
  rownames(Y_all) <- names(mu_1)
  mode(Y_all) <- "integer"

  X_all <- rep(c(-1,1), each = nSamples/2)


  return(list("Y" = Y_all, "X" = X_all))

}
