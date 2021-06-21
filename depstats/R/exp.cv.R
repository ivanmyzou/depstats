#' Exponential Curves
#'
#' Creates a bivariate sample belonging to the exponential curves dependence structure
#' @param n size of the sample (number of bivariate data pairs).
#' @param b (by default \eqn{2}) specifies the exponential base.
#' @examples
#' X <- exp.cv(1000)
#' qplt(X)
#'

exp.cv <- function(n,b=2){
  u = runif(n,-1,1)
  x = u
  y = b^x
  X = cbind(x,y)
  return(X)
}
