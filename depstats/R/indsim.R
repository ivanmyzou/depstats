#' Random Estimates of Measures over Independent Samples
#'
#' @param f function of any measure or dependence test in the package.
#' @param n size of the sample (number of bivaraite data pairs).
#' @param runs numbers of repetitions applied.

indsim <- function(f,n = 100,runs = 1000){
  sim <- c()
  for (i in 1:runs){
    sim[i] <- f(cbind(runif(n),runif(n)))
  }
  return(sim)
}
