#' Applications over Samples
#'
#'
#' @description This function is used to compute features (competitor scores or image greyscale pixels)
#' @param X the matrix of random sample.
#' @param size a vector of sample sizes
#' @param option (0/1) 0 for greyscale computation and 1 for competitor scores computation.
#' @param grid dimension of the square grid of grescale image pixels.
#' @param printall (TRUE/FALSE) default being FALSE would give a progress bar, if TRUE prints out the number progress.
#' @name sampleapply


library(foreach) #efficient for looping

sampleapply <- function(X,size,option,grid = 25,printall = FALSE){
  n <- nrow(X) %/% sum(size) #number of repetition over size
  s <- rep(size,n)
  cs <- c(0,cumsum(s))
  if (option == 0){ #computation of greyscale pixel

    if (printall == FALSE){ #progress bar
      pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) ETA: :eta",
                                       total = (length(size)*n))
    }

    Y <- foreach(i=1:(length(size)*n), .combine='rbind') %do% {
      index <- (cs[i]+1):cs[i+1]
      Xrank <- srank(X[index,])
      H <- MASS::kde2d(Xrank[,1], Xrank[,2], n = grid)
      v <- c(t(H[['z']]))

      if (printall == TRUE){ #progress
        print(i)
      } else{ pb$tick() }

      return(round(v/max(v), digits = 5))
    }

  } # end of option 0

  if (option == 1){ #computation of competitor scores
    N <- length(size)*n

    if (printall == FALSE){ #progress bar
      pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) ETA: :eta",
                                       total = N)
    }

    Y <- foreach(i=1:N, .combine = 'rbind') %do% {
      index <- (cs[i]+1):cs[i+1]
      Z <- X[index,] #Z is the current sample worked with

      if (printall == TRUE){ #progress
        print(i)
      } else{ pb$tick() }

      return( c(mace(Z),mauk(Z),mblom(Z),mdist(Z),
                mhell(Z),mhoeff(Z),mhsic(Z),
                minf(Z),mken(Z),mmdif(Z),mmic(Z),
                mrand(Z),mspear(Z),
                pddr(Z),phhg(Z))
      )
    }

  } # end of option 1

  return(Y)
}
