#' Quick Plotting
#'
#' @param X the matrix of random sample.
#' @param col color of the plot.
#'

qplt <- function(X,col='navy',xy='n'){
  plot(X[,1],X[,2], xlab = '', ylab = '', pch=19, col=col, xaxt=xy,yaxt=xy)
}
