#' Blomqvist Correlation
#'
#'@references
#' BlomqvistN., (1950),On a Measure of Dependence between Two Random Variables, Ann.Math. Statist., 21(4), 1950, 593-600.

mblom <- function(X){
  x <- X[,1]; y <- X[,2]
  medx <- median(x); medy <- median(y)
  n <- nrow(X)
  if (n %% 2 == 0){
    n1 <- sum( (x > medx) * (y > medy) + (x < medx) * (y < medy) )
    n2 <- sum( (x > medx) * (y < medy) + (x < medx) * (y > medy) )
    if (n1 + n2 == n - 2){
      ytied <- y[which(x == medx)]
      xtied <- x[which(y == medx)]
      n1 <- n1 + ( (xtied > medx) * (ytied > medy) + (xtied < medx) * (ytied < medy) )
      n2 <- n2 + ( (xtied > medx) * (ytied < medy) + (xtied < medx) * (ytied > medy) )
    }
  }
  m <- abs(n1-n2)/(n1+n2)
  return(m)
}
