#' Area Under Kendall Plot
#'
#'@references
#' Vexler A.; Chen X.; Hutson A. D., (2017), Dependence and Independence Structure and Inference, StatMethodsMed Res., 2017 Oct;26(5):2114-2132.

mauk <- function(X){
  x <- X[,1]; y <- X[,2]
  m <- (testforDEP::AUK(x,y)$AUK - 0.5)^2
  return(m)
}
