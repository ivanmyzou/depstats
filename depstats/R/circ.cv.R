#' Circular Curves
#'
#' Creates a bivariate sample belonging to the circular curves dependence structure
#' @param n size of the sample (number of bivariate data pairs).
#' @param cen a \eqn{m*2} matrix where each of its row specifies a centre of the circle (by default only the origin (0,0)).
#' @param r (by default \eqn{1}) the radius of the circles.
#' @examples
#' X <- circ.cv(1000,matrix(c(1,1,-1,-1,0,0), byrow=TRUE, ncol=2))
#' qplt(X)
#' @references
#' Geenens G. & Lafaye de Micheaux P., (2018),
#' The Hellinger Correlation,
#' arXiv:1810.10276.

circ.cv <- function(n,cen=matrix(c(0,0), byrow = TRUE, ncol = 2),r=1){
  u = runif(n,-1,1)
  choice = sample(seq(1,nrow(cen)), size = n, replace = T) #random sampling on circles
  x = r*sin(u * pi) + cen[choice, 1]
  y = r*cos(u * pi) + cen[choice, 2]
  X = cbind(x,y)
  return(X)
}
