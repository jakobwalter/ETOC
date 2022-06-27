PrintProgress <- function(i, n, t0){
  ### simple helper function that computes the time remaining
    cat("\r", 
        "Progress: ", round(i/n,3),
        "Time Remaining: ", 
        round(difftime(Sys.time(), t0, units = "mins")/(i/n)*(1- i/n),2), " min    ")
  return()
}