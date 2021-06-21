#' Data--Driven Rank Test
#'
#'@references
#' Kallenberg W. C. M.; Ledwina T., (1999) Data-Driven Rank Tests for Independence, Journal of the American Statistical Association, 94(445), 285-301.

pddr <- function(X,num.MC=100){
  x <- X[,1]; y <- X[,2]
  p <- c()
  p[1] <- 1 - testforDEP::testforDEP(x,y,test='V',num.MC = num.MC)@p_value
  p[2] <- 1 - testforDEP::testforDEP(x,y,test='TS2',num.MC = num.MC)@p_value
  return(p)
}
