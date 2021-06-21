#' Martingale Difference Correlation
#'
#'@references
#' Shao X. & Zhang J., (2014), Martingale Difference Correlation and Its Use in High-Dimensional Variable Screening, Journal of the American Statistical Association, 109(507), 2014.

mmdif <- function(X, c = 'U'){
  Y <- srank(X)
  m <- ( abs(EDMeasure::mdc(Y[,1],Y[,2],center = c)) +
         abs(EDMeasure::mdc(Y[,2],Y[,1],center = c)) )/2
  return(m)
}
