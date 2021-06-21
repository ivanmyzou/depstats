#' Dependent Sample Generation
#'
#' @param num number of repetitions applied.
#' @param n size of the sample (number of bivaraite data pairs).
#' @param expr a string describing the code to generate a dependent sample.
#' @param randrotate (TRUE/FALSE) default being TRUE to rotate the sample randomly.
#' @param print (TRUE/FALSE) default being FALSE to print out the number progress.

depgen <- function(num,n,expr,randrotate=TRUE,print=FALSE){
  X <- matrix(nrow=num*n,ncol=2)
  if (randrotate==TRUE){
    for (i in 1:num){
      X[ ((i-1)*n+1):(i*n), ] <- rotate(eval(parse(text=expr)))
      if (print==TRUE){
        print(i)
      }
    }
  }else{
    for (i in 1:num){
      X[ ((i-1)*n+1):(i*n), ] <- eval(parse(text=expr))
      if (print==TRUE){
        print(i)
      }
    }
  }
  return(X)
}

