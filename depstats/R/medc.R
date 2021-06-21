#' Estimated Copula Dependence Measure
#'
#'@references
#' Jiang H. J. &Wu Q. L., (2018), Robust Dependence Measure for Detecting Associations in Large Data Set, Acta.Math. Sci., 38(1), 931-949.

medc <- function(X){
  x <- X[,1]
  y <- X[,2]
  uy <- ecdf(as.matrix(y))(as.matrix(y))
  ux <- ecdf(as.matrix(x))(as.matrix(x))
  m1 <- sqrt(abs(acepack::ace(ux,uy)$rsq))
  m2 <- sqrt(abs(acepack::ace(uy,ux)$rsq))
  m <- (m1+m2)/2
  return(m)
}
