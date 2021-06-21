#' Wedge Curves
#'
#' Creates a bivariate sample belonging to the wedge dependence structure
#' @param n size of the sample (number of bivariate data pairs).
#' @examples
#' X <- wedge.cv(1000)
#' qplt(X)
#' @references
#' Geenens G. & Lafaye de Micheaux P., (2018),
#' The Hellinger Correlation,
#' arXiv:1810.10276.

wedge.cv <- function(n){
  u = runif(n,-1,1)
  x = u
  y = 2*(x+1) * (sample(c(-1, 1), size = n, replace = T)) #high or low
  X = cbind(x,y)
  return(X)
}
