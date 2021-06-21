#' Triangle Clouds
#'
#' Creates a bivariate sample belonging to the triangle clouds dependence structure
#' @param n size of the sample (number of bivaraite data pairs).
#' @param l,r extent of span towards the left and the right of the triangle.
#' @examples
#' X <- tri.cld(1000)
#' qplt(X)


tri.cld <- function(n,l=1,r=1){
  v = runif(n, min = 0, max = 1)
  u = c()
  for (i in 1:length(v)){
    u[i] = runif(1, min = l*(-v[i]), max = r*(v[i])) #range changes based on height
  }
  X = cbind(u,v)
  return(X)
}
