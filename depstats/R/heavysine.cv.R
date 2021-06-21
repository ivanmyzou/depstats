#' Heavy Sine Curves
#'
#' Creates a bivariate sample belonging to the heavy-sine curves dependence structure
#' @param n size of the sample (number of bivariate data pairs).
#' @param a,b,c,d specifies the cut of heavy sines (by default \eqn{-0.6,-0.2,0.2,0.6}).
#' @examples
#' X <- heavysine.cv(1000)
#' qplt(X)
#' @references
#'Jiang H. J. & Wu Q. L., (2018),
#'Robust Dependence Measure for Detecting Associations in Large Data Set,
#'Acta. Math. Sci., 38(1), 931-949.

heavysine.cv <- function(n,a=-0.6,b=-0.2,c=0.2,d=0.6){
  u = runif(n,-1,1)
  x = u
  y = 1/2*sin(4*pi*x) - 1/8*sign(x-a) - 1/8*sign(x-b) - 1/8*sign(x-c) - 1/8*sign(x-d) #cuts added
  X = cbind(x,y)
  return(X)
}
