#' W-Shape Curves
#'
#' Creates a bivariate sample belonging to the w-shape dependence structure
#' @param n size of the sample (number of bivariate data pairs).
#' @examples
#' X <- w.cv(1000)
#' qplt(X)
#' @references
#' Geenens G. & Lafaye de Micheaux P., (2018),
#' The Hellinger Correlation,
#' arXiv:1810.10276.

w.cv <- function(n){
  u = runif(n,-1,1)
  x = u
  y = 4 * ((x^2 - 1/2)^2) #quadratic polynomial
  X = cbind(x,y)
  return(X)
}
