#' Feature Importation
#'
#' @param scores txt file with competitor scores.
#' @param image txt file with image greyscale pixel values.
#' @param n sample size.


importfeatures <- function(scores,image,n){
  if (grepl('.txt$',scores)){
    M <- read.table(scores)
  } else if (grepl('.fst$',scores)){
    M <- read_fst(scores)
  }
  if (grepl('.txt$',image)){
    I <- read.table(image)
  } else if (grepl('.fst$',image)){
    I <- read_fst(image)
  }
  return( cbind(M,rep(n,nrow(M)),I) )
}
