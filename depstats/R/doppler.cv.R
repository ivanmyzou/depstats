#' Doppler Curves
#'
#' Creates a bivariate sample belonging to the doppler curves dependence structure
#' @param n size of the sample (number of bivariate data pairs).
#' @examples
#' X <- doppler.cv(1000)
#' qplt(X)
#' @references
#' Geenens G. & Lafaye de Micheaux P., (2018),
#' The Hellinger Correlation,
#' arXiv:1810.10276.

doppler.cv <- function(n,f=4){
  u = runif(n,-1,1)
  x = u
  y = sin(-f*(1+x)*(2+x)) # f accelerates frequency
  X = cbind(x,y)
  return(X)
}

