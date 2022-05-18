GetPreds <- function(models, cutoffs){
  
  newd <- data.frame(mu = seq(cutoffs$mu[1], cutoffs$mu[2], length.out = 200),
                     phi = seq(cutoffs$phi[1], cutoffs$phi[2], length.out = 200),
                     corrMax = seq(cutoffs$corrMax[1], cutoffs$corrMax[2],
                                   length.out = 200)
  )
  
  margin.terms <- all.vars(models[[1]]$formula)[2:4]
  model.terms <- paste0("s(", all.vars(models[[1]]$formula)[2:4], ")")
  
  model.terms <- c("s(log(mu))", "s(phi)", "s(corrMax)")
  
  
  preds <- lapply(1:length(model.terms), function(i){
    pred <- lapply(models, function(m){
      mgcv::predict.gam(m, newd, exclude = model.terms[-i], type = "response")
    })
    
    pred <- as.data.frame(pred)
    colnames(pred) <- names(models)
    pred$x <- newd[[margin.terms[i]]]
    pred <- tidyr::pivot_longer(pred, cols = 1:length(models))
    return(pred)
  })
  names(preds) <- margin.terms
  return(preds)
  
}