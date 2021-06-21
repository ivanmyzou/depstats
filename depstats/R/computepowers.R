#' Compute Powers of All Competitors
#'
#' @name computepowers
#' @param features features obtained from importfeatures().
#' @param model string indicating the model ('allnet' or 'allCNN')
#' @param Model keras model object
#' @param n sample size.

library(foreach)

computepowers <- function(features,model,Model,n){
  Array <- featurereshape(features,model)
  if (model=='allnet'){
    p <- predict(Model, Array)
    modelpower <- sum(p >= NETthres[sprintf('%s',n)]) / nrow(features)
  }
  else if (model=='allCNN'){
    p <- predict(Model, Array)
    modelpower <- sum(p[,2] >= CNNthres[sprintf('%s',n)]) / nrow(features)
  }
  else if (model=='scoreonly'){
    p <- predict(Model, Array)
    modelpower <- sum(p >= SCOREthres[sprintf('%s',n)]) / nrow(features)
  }
  else if (model=='imageonly'){
    p <- predict(Model, Array)
    modelpower <- sum(p[,2] >= IMAGEthres[sprintf('%s',n)]) / nrow(features)
  }
  else if (model=='scores'){#the first 20 columns are scores
    modelpower <- foreach(i=1:20,.combine='c') %do% {
      Mpower <- sum(features[,i] >= OTHERthres[sprintf('%s',n)][[1]][i]) / nrow(features)
      return(Mpower)
    }
  }
  return(modelpower)
}
