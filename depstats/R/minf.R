#' Information Correlation Coefficient
#'
#'@references
#' Hausser J. & Strimmer K., (2015), Estimation of Entropy, Mutual Information and Related Quantities, R package, Version 1.2.1.

minf <- function(X){
  Y <- srank(X)
  m <- sqrt(1 - exp(- 2* max(mpmi::cminjk(Y)[1,2],0) )) #mutual information cannot be negative
  return(m)
}
