#' Heart Curves
#'
#' Creates a bivariate sample belonging to the heart curves dependence structure
#' @param n size of the sample (number of bivariate data pairs).
#' @examples
#' X <- heart.cv(1000)
#' qplt(X)
#' @references
#'Jiang H. J. & Wu Q. L., (2018),
#'Robust Dependence Measure for Detecting Associations in Large Data Set,
#'Acta. Math. Sci., 38(1), 931-949.

heart.cv <- function(n){
  u = runif(n,-1,1)
  x = 1/2*(2*cos(pi*u)-cos(2*pi*u))
  y = 1/2*(2*sin(pi*u)-sin(2*pi*u))
  X = cbind(x,y)
  return(X)
}
