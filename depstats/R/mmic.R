#' Maximal Information Coefficient
#'
#'@references
#' Filosi M.; Visintainer R.; Albanese D.; Riccadonna S.; Jurman G.; Furlanello C., (2019), Maximal Information-Based Nonparametric Exploration for Variable Analysis, R package, Version 1.5.8.

mmic <- function(X){
  x <- X[,1]; y <- X[,2]
  m <- minerva::mine(x,y)$MIC
  return(m)
}
