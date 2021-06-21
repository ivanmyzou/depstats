#' Spiral Curves
#'
#' Creates a bivariate sample belonging to the spiral curves dependence structure
#' @param n size of the sample (number of bivariate data pairs).
#' @param k intensity of spirals.
#' @examples
#' X <- spiral.cv(1000)
#' qplt(X)
#' @references
#'Jiang H. J. & Wu Q. L., (2018),
#'Robust Dependence Measure for Detecting Associations in Large Data Set,
#'Acta. Math. Sci., 38(1), 931-949.

spiral.cv <- function(n,k=2){
  u = runif(n,-k*pi,k*pi) # k controls number of windings
  x = exp(- u / 10) * cos(u)
  y = exp(- u / 10) * sin(u)
  X = cbind(x,y)
  return(X)
}
