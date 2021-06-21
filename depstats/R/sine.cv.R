#' Sine Curves
#'
#' Creates a bivariate sample belonging to the sine curves dependence structure
#' @param n size of the sample (number of bivariate data pairs).
#' @param s frequency of sine waves.
#' @examples
#' X <- sine.cv(1000)
#' qplt(X)
#' @references
#' Geenens G. & Lafaye de Micheaux P., (2018),
#' The Hellinger Correlation,
#' arXiv:1810.10276.

sine.cv <- function(n,s=2){
  u = runif(n,-1,1)
  x = u
  y = sin(s*pi*x) # s accelerates frequency
  X = cbind(x,y)
  return(X)
}
