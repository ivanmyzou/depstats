#' Points Clouds
#'
#' Creates a bivariate sample belonging to the points clouds dependence structure
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

pt.cld <- function(n,X=matrix(c(0,0),ncol=2,s)){
  choice = sample(seq(1,nrow(X)), size = n, replace = T) #sampling on points
  x = X[choice, 1]
  y = X[choice, 2]
  Y = cbind(x,y)
  return(Y)
}
