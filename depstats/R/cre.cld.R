#' Crescent Clouds
#'
#' Creates a bivariate sample belonging to the crescent clouds dependence structure
#' @param n size of the sample (number of bivariate data pairs).
#' @param c (by default \eqn{0.5}) waxes/wanes of the crescent shape.
#' @return matrix X with two columns representing the bivariate sample.
#' @examples
#' X <- cre.cld(1000)
#' qplt(X)


cre.cld <- function(n,c=0.5){
  v = runif(n, min = -1, max = c/2)
  u = c()
  for (i in 1:length(v)){
    if (v[i] < c-1){
      u[i] = runif(1, min = -sqrt(1 - v[i]^2), max = sqrt(1 - v[i]^2))
    }else{
      ind = sample(c(0,1))
      a = t(c(-sqrt(1 - v[i]^2), sqrt(1 - (v[i]-c)^2))) %*% ind
      b = t(c(-sqrt(1 - (v[i]-c)^2), sqrt(1 - v[i]^2))) %*% ind
      u[i] = runif(1, min = a, max = b) #between two parabolas
    }
  }
  X = cbind(u,v)
  return(X)
}
