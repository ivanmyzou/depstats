#' Sample Generator from Image
#'
#' Produce a sample with probability determined by greyscale pixels of an image
#' @param n size of the sample (number of bivaraite data pairs).
#' @param image an image object (under imager package).


randimage <- function(n,image){
  DF <- as.data.frame(imager::grayscale(image))
  xwidth <- max(DF$x)
  ywidth <- max(DF$y)
  x <- (DF$x - xwidth/2)/xwidth
  y <- -(DF$y - ywidth/2)/ywidth
  val <- (1-DF$value)/sum(DF$value)
  index = sample.int(length(val), n, replace = TRUE, prob=val)
  X <- cbind(x[index],y[index])
  return(X)
}
