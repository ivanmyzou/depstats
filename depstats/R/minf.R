#' Information Correlation Coefficient
#'
#'@references
#' Hausser J. & Strimmer K., (2015), Estimation of Entropy, Mutual Information and Related Quantities, R package, Version 1.2.1.

minf <- function(X,b1=10,b2=10){
  Y <- srank(X)
  X2d = entropy::discretize2d(Y[,1], Y[,2], numBins1=b1, numBins2=b2)
  m <- sqrt(1 - exp(- 2* entropy::mi.empirical(X2d)) )
  return(m)
}
