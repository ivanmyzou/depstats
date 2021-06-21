#' Cross Curves
#'
#' Creates a bivariate sample belonging to the cross curves dependence structure
#' @param n size of the sample (number of bivariate data pairs).
#' @param g (by default \eqn{1}) angle of rotation to obtain the cross shape.
#' @examples
#' X <- cross.cv(1000)
#' qplt(X)
#' @references
#' Geenens G. & Lafaye de Micheaux P., (2018),
#' The Hellinger Correlation,
#' arXiv:1810.10276.

cross.cv <- function(n,g=1){
  u = runif(n,-1,1)
  x = u
  y = g*(x) * (sample(c(-1, 1), size = n, replace = T)) #two branches
  X = cbind(x,y)
  return(X)
}
