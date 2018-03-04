actual_vs_fitted <- function(lm_model = NULL, dates ,actual = NULL,fitted = NULL){

  if(!is.null(lm_model)){

    actual <- lm_model$model[,1]

    fitted <- lm_model$fitted.values

  }

  data.frame(Date = dates, Actual = actual, Fitted = fitted, stringsAsFactors = F)
}
