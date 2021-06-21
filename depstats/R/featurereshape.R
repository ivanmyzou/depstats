#' Feature Reshaping (for keras models)
#'
#' @name featurereshape
#' @param features features obtained from importfeatures().
#' @param model string indicating the model ('allnet' or 'allCNN')

library(foreach)

featurereshape <- function(features,model){
  if (model=='allnet'){
    Array <- keras::array_reshape(unlist(features),c(nrow(features),646),
                                  order = c("F")) #each row represents a data sample
    return(Array)
  }
  else if (model=='allCNN'){
    Array <- array(NaN,c(nrow(features),26,25,1)) #each row represents a data sample
    pb <- progress::progress_bar$new(format=" reshaping for allCNN [:bar] :percent ETA: :eta", total = nrow(features))
    nothing <- foreach(i=1:nrow(features),.combine='c') %do% {
      feature <- unlist(features[i,])
      featurezero <- c(feature[1:21],rep(0,4),feature[22:length(feature)])
      Array[i,,,] <- keras::array_reshape(featurezero,c(1,26,25,1))
      pb$tick()
    }
    return(Array)
  }
  else if (model=='scoreonly'){
    Array <- keras::array_reshape(unlist(features[,1:21]),c(nrow(features),21),
                                  order = c("F")) #each row represents a data sample
    return(Array)
  }
  else if (model=='imageonly'){
    Array <- array(NaN,c(nrow(features),26,25,1)) #each row represents a data sample
    pb <- progress::progress_bar$new(format=" reshaping for imageonly [:bar] :percent ETA: :eta", total = nrow(features))
    nothing <- foreach(i=1:nrow(features),.combine='c') %do% {
      feature <- unlist(features[i,])
      featurezero <- c(rep(0,24),feature[21],feature[22:length(feature)])
      Array[i,,,] <- keras::array_reshape(featurezero,c(1,26,25,1))
      pb$tick()
    }
    return(Array)
  }
}
