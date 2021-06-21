#' Distance Correlation
#'
#'@references
#' Richards D. S. P., (2017), Distance Correlation: A New Tool for Detecting Association and Measuring Correlation between Data Sets, arXiv:1709.06400.

mdist <- function(X){
  Y <- srank(X)
  m <- energy::dcor(Y[,1], Y[,2])
  return(m)
}
