#' Cubic Curves
#'
#' Creates a bivariate sample belonging to the cubic curves dependence structure
#' @param n size of the sample (number of bivariate data pairs).
#' @examples
#' X <- cubic.cv(1000)
#' qplt(X)
#' @references
#' Geenens G. & Lafaye de Micheaux P., (2018),
#' The Hellinger Correlation,
#' arXiv:1810.10276.

cubic.cv <- function(n){
  u = runif(n,-1,1)
  x = u
  y = x^3 - x #cubic polynomial
  X = cbind(x,y)
  return(X)
}
