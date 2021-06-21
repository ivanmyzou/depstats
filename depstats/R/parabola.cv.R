#' Parabola Curves
#'
#' Creates a bivariate sample belonging to the parabola curves dependence structure
#' @param n size of the sample (number of bivariate data pairs).
#' @examples
#' X <- parabola.cv(1000)
#' qplt(X)

parabola.cv <- function(n){
  u = runif(n,-1,1)
  x = u
  y = (x^2)/2 #parabola
  X = cbind(x,y)
  return(X)
}
