#' Normalised Spearman Correlation Coefficient
#'
#'@references
#'The R Base Package

mspear <- function(X){
  m <- abs(cor(x = X[,1], y = X[,2], method = 'spearman'))
  return(m)
}
