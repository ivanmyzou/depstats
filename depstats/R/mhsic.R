#' Hilbert Schmidt Independence Criterion
#'
#'@references
#' Gretton A.; Bousquet O.; Smola A.; Schölkopf B., (2005), Measuring Statistical Dependence with Hilbert-Schmidt Norms, ALT’05 Proceedings of the 16th international conference on Algorithmic Learning Theory, 63-77.

mhsic <- function(X, k = 'gaussian'){
  Y <- srank(X)
  m <- dHSIC::dhsic(list(Y[,1],Y[,2]), kernel = k)$dHSIC
  return(m)
}
