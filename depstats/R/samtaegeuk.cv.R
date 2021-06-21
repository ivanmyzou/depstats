#' Samtaegeuk Curves
#'
#' Creates a bivariate sample belonging to the samtaegeuk curves dependence structure
#' @param n size of the sample (number of bivaraite data pairs).
#' @examples
#' X <- samtaegeuk.cv(1000)
#' qplt(X)

samtaegeuk.cv <- function(n){
  u = runif(n,-1,1)#Sample A
  delta <- sample(c(0, 1, 2, 3), size = n, replace = T, prob = c(1/2,1/6,1/6,1/6))
  x = cos(u * pi) * (delta == 0) +
    1/2*cos(u * pi/2) * (delta == 1) +
    (1/2*cos(u * pi/2 + pi/2 + pi/6) + sqrt(3)/4) * (delta == 2) +
    (1/2*cos(- u * pi/2 - pi/2 - pi/6) - sqrt(3)/4) * (delta == 3)
  y = sin(u * pi) * (delta == 0) +
    (1/2*sin(u * pi/2) - 1/2 )* (delta == 1) +
    (1/2*sin(u * pi/2 + pi/2 + pi/6) + 1/4 ) * (delta == 2) +
    (1/2*sin(- u * pi/2 - pi/2 - pi/6) + 1/4) * (delta == 3)
  X = cbind(x,y)
  return(X)
}
