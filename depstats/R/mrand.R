#' Randomised Dependence Coefficient
#'
#'@references
#' Lopez-PazD.;Hennig P.; Schölkopf B., (2013) The Randomised Dependence Coefficient, arXiv:1304.7717.

mrand <- function(X,k=10,s=1/2,Phi=c(sin,cos),seed) {
  if (! missing(seed)) {set.seed(seed)}
  Y <- srank(X)
  x <- Y[,1]; y <- Y[,2]
  x <- cbind(as.matrix(x), 1)
  y <- cbind(as.matrix(y), 1)
  x <- (x/ncol(x)) %*% matrix(rnorm(ncol(x)*k), ncol(x)) * s
  y <- (y/ncol(y)) %*% matrix(rnorm(ncol(y)*k), ncol(y)) * s
  if (typeof(Phi) == 'list'){
    PhiX <- c()
    PhiY <- c()
    for (i in length(Phi)){
      PhiX <- cbind(PhiX,Phi[i][[1]](x))
      PhiY <- cbind(PhiY,Phi[i][[1]](y))
    }
    PhiX <- cbind(PhiX,1)
    PhiY <- cbind(PhiY,1)
  } else {
    PhiX <- cbind(Phi(x),1)
    PhiY <- cbind(Phi(y),1)
  }
  m <- cancor(PhiX,PhiY)$cor[1]
  return(m)
}
