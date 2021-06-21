#' Independent Uniform Bivariate Sample Generator
#'
#' @param n numbers of replication produced.
#' @param size a vector of sample sizes
#' @examples
#' X <- indgen(n=10, sizes = c(50,100,200,500,1000))
#' #10 copies of sample sizes 50,100,200,500,1000 each created
#' #statistically the same as
#' X <- indgen(n=10*sum(50,100,200,500,1000), sizes = c(1))

indgen <- function(n, sizes = c(50,100,200,500,1000)){
  N <- n * sum(sizes)
  X <- matrix(nrow=N,ncol=2)
  X[,1] <- runif(N,-1,1)
  X[,2] <- runif(N,-1,1)
  return(X)
}
