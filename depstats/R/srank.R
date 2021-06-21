#' Normalised Sample Ranks
#'
#' Takes a sample and applies ranks on the two random variables
#'

srank <- function(X){
  if (is.null(ncol(X))){
    Y <- 2*(X-median(X))/length(X)
  } else{
    x1 <- rank(X[,1])
    x2 <- rank(X[,2])
    Y <- cbind( 2*(x1-median(x1))/length(x1), 2*(x2-median(x2))/length(x2) )
  }
  return(Y)
}
