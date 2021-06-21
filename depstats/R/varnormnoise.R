#' Adding Normally Ditributed Noises with Non-Constant Variance
#'
#' @param X the matrix of random sample.
#' @param expr a string with its absolute value describing the variance function.

varnormnoise <- function(X,expr='0.1'){
  x = X[,1]
  epsilonx = 0
  epsilony = rnorm(nrow(X), 0, abs(eval(parse(text=expr))))
  Y = X + cbind(epsilonx,epsilony)
  return(Y)
}
