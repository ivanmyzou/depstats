#' Model Prediction
#'
#' @description This function is used to compute model predictions from either image, score or combined model.
#' @param model the model object.
#' @param model_name a string of model name, must be either `'image'`, `'score'` or `'combined'.`
#' @param df the input data for model prediction.
#' @name model_predict

model_predict <- function(model, model_name, df) {
  if (model_name == 'image'){ #image only model
    x <- array_reshape(unlist(df[,21:ncol(df)]), c(nrow(df),25,25,1),
                       order = c("F"))
    #transposing
    for (i in 1:dim(x)[1]) {
      x[i,,,1] <- t(x[i,,,1])
    }
    pred <- predict(model, x)
  } else if (model_name == 'score'){ #score only model
    x <- array_reshape(unlist(df[,1:20]), c(nrow(df),20),
                       order = c("F"))
    pred <- predict(model, x)
  } else if (model_name == 'combined'){ #combined model
    x <- array_reshape(unlist(df), c(nrow(df),ncol(df)),
                       order = c("F"))
    pred <- predict(model, x)[,2]
  }
  return(pred)
}
