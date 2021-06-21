#' Heller-Heller-Gorfine Test
#'
#'@references
#' Heller R.; Heller Y; GorfineM., (2012), A Consistent Multivariate Test of Association Based on Ranks of Distances, arXiv:1201.3522.

phhg <- function(X,nperm = 50){
  x <- as.matrix(dist((srank(X[,1])), diag = TRUE, upper = TRUE))
  y <- as.matrix(dist((srank(X[,2])), diag = TRUE, upper = TRUE))
  m <- HHG::hhg.test(x,y,nr.perm = nperm)
  p <- 1 - c(m$perm.pval.hhg.sc,
             m$perm.pval.hhg.sl,
             m$perm.pval.hhg.mc,
             m$perm.pval.hhg.ml )
  return(p)
}
