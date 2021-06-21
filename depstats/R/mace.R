#' Alternating Conditional Expectations
#'
#'@references
#' Spector P.; Friedman J.; Tibshirani R.; Lumley T.; Garbett S.; Baron J., (2019), ACE and AVAS for Selecting Multiple Regression Transformations, R package, Version 1.4.1.

mace <- function(X){
  Y <- srank(X)
  x <- Y[,1]; y <- Y[,2]
  m <- ( abs(acepack::ace(x, y)$rsq) + abs(acepack::ace(y, x)$rsq) )/2
  return(m)
}
