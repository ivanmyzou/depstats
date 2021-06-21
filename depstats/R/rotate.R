#' The Rotation Function
#'
#' Anti-clockwise rotation of a sample around the origin
#' @param X the matrix of random sample.
#' @param theta radian of rotation.

rotate <- function(X,theta){
  if (missing(theta)){ theta <- runif(1,0,2*pi)}
  R = matrix( c(cos(theta), sin(theta), -sin(theta), cos(theta)) , nrow = 2)
  Y = t(R %*% t(X))
  return(Y)
}
