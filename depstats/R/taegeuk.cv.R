#' Taegeuk Curves
#'
#' Creates a bivariate sample belonging to the taegeuk curves dependence structure
#' @param n size of the sample (number of bivariate data pairs).
#' @examples
#' X <- taegeuk.cv(1000)
#' qplt(X)
#' @references
#'Jiang H. J. & Wu Q. L., (2018),
#'Robust Dependence Measure for Detecting Associations in Large Data Set,
#'Acta. Math. Sci., 38(1), 931-949.

taegeuk.cv <- function(n){
  u = runif(n,-1,1)
  delta <- sample(c(-1, 0, 1), size = n, replace = T, prob = c(1/4, 1/2, 1/4))
  x = (1/2)^abs(delta)*sin(pi*u)  + 1/2*delta
  y = abs( (1/2)*cos(pi*u) ) * delta + cos(pi*u) * (1 - abs(delta))
  X = cbind(x,y)
  return(X)
}
