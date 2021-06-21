#' Adding Normally Distributed Noises
#'
#' @param X the matrix of random sample.
#' @param sigma the standard deviation of the normal noise added.

normnoise <- function(X,sigma,both=FALSE){
  epsilony = rnorm(nrow(X), 0, sigma)
  if (both == TRUE){
    epsilonx = rnorm(nrow(X), 0, sigma)
  } else{
    epsilonx = 0
  }
  Y = X + cbind(epsilonx,epsilony)
  return(Y)
}
