#' Two Parabolae Curves
#'
#' Creates a bivariate sample belonging to the two-parabolae dependence structure
#' @param n size of the sample (number of bivaraite data pairs).
#' @examples
#' X <- twoparabola.cv(1000)
#' qplt(X)
#' @references
#' Geenens G. & Lafaye de Micheaux P., (2018),
#' The Hellinger Correlation,
#' arXiv:1810.10276.

twoparabola.cv <- function(n){
  u = runif(n,-1,1)
  x = u
  y = (x^2) * (sample(c(-1, 1), size = n, replace = T)) #upper and lower
  X = cbind(x,y)
  return(X)
}
