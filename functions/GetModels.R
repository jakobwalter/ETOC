GetModels <- function(t1Data, formula, sp = c(0.05, 1, 0.9)){
  paramGrid <- expand.grid(Model = unique(t1Data$name),
                           n =  as.character(unique(t1Data$n)),
                           DE = c(T,F))
  fittedModels <- apply(paramGrid, 1, function(p){
    mask <- t1Data$n == p["n"] & t1Data$deMask == p["DE"] & t1Data$name == p["Model"]
    b1 <- mgcv::gam(formula, data = t1Data[mask,],
                    family = mgcv::betar(eps = 0.00001), sp = sp)
    
  })
  names(fittedModels) <- paste(paramGrid$Model, paramGrid$n, ifelse(paramGrid$DE, "DE", "NDE"), sep = "_")
  return(fittedModels)
}