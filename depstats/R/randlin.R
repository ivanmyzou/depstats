#' Linear Lines with Random Gradient
#'
#' @param n size of the sample (number of bivariate data pairs).
#' @param a lower bound of the gradient.
#' @param b upper bound of the gradient.

randlin <- function(n,a=-2,b=2){
  direc <- sample(c(0,1),1)
  grad <- direc*runif(1,a,-1/2) + (1-direc)*runif(1,1/2,b)
  x <- runif(n,-1,1)
  y <- grad*x
  X <- cbind(x,y)
  return(X)
}
