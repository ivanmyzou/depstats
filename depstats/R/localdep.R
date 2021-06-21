#' Local Dependence Generator
#'
#' Takes a sample and add noises to the sides
#' @param X the matrix of random sample.
#' @param prop proportion of the sample randomised.
#' @examples
#' X <- cross.cv(1000)
#' qplt(localdep(X,0.75))
#'

localdep <- function(X,prop){
  upto <- round(nrow(X)*(1-prop))
  Y <- X[1:upto,]
  add <- nrow(X) - nrow(Y)
  index <- sample(c(0,1),add,replace=TRUE)
  x <- index * runif(add,1,2) + (1-index) * runif(add,-2,-1)
  y <- runif(add,-2,2)
  return( rbind(Y,cbind(x,y)) )
}
