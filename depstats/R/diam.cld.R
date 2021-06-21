#' Diamond Clouds
#'
#' Creates a bivariate sample belonging to the diamond clouds dependence structure
#' @param n size of the sample (number of bivariate data pairs).
#' @param theta (by default \eqn{\pi}) angle of rotation to obtain the diamond shape.
#' @return matrix X with two columns representing the bivariate sample.
#' @examples
#' X <- diam.cld(1000)
#' qplt(X)
#' @references
#' Geenens G. & Lafaye de Micheaux P., (2018),
#' The Hellinger Correlation,
#' arXiv:1810.10276.

diam.cld <- function(n,theta=pi/4){
  u = runif(n, min = -1, max = 1)
  v = runif(n, min = -1, max = 1)
  W = cbind(u,v)
  R = rbind(c(cos(theta), -sin(theta)), c(sin(theta), cos(theta))) #rotation
  X =  t(R %*% t(W))
  return(X)
}
