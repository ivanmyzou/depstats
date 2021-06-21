#' Hellinger Correlation
#'
#'@references
#' Geenens G. & Lafaye de Micheaux P., (2018), The Hellinger Correlation, arXiv:1810.10276.

mhell <- function(X){
  m <- HellCor::HellCor(X[,1], X[,2])$Hcor
  return(m)
}
