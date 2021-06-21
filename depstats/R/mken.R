#' Normalised Kendall Rank Correlation Coefficient
#'
#'@references
#'The R Base Package

mken <- function(X){
  m <- abs(cor(x = X[,1], y = X[,2], method = 'kendall'))
  return(m)
}
