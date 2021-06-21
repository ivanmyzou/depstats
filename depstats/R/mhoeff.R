#' Hoeffding Dependence Coefficient
#'
#'@references
#' Hoeffding W., (1948), A Non-Parametric Test of Independence, Ann. Math. Statist., 19(4), 1948, 546-557.

mhoeff <- function(X){
  x <- X[,1]; y <- X[,2]
  n <- nrow(X)
  R <- rank(x); S <- rank(y)
  Q <- c()
  for (i in 1:n){
    Q[i] <- sum((x[i] > x) * (y[i] > y)) + 1
  }
  D1 <- sum((Q-1)*(Q-2))
  D2 <- sum((R-1)*(R-2)*(S-1)*(S-2))
  D3 <- sum((R-2)*(S-2)*(Q-1))
  m <- 30 * abs( (n-2)*(n-3)*D1 + D2 - 2*(n-2)*D3 )/( n*(n-1)*(n-2)*(n-3)*(n-4) )
  return(m)
}
