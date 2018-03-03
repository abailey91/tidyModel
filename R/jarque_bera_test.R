jarque_bera_test <- function(lm_model=NULL,residuals = NULL,n_regressors = NULL){

  if(!is.null(lm_model)){
    x <- lm_model$residuals
    k <- length(variable.names(lm_model)) - 1 #remove intercept term
  }else{
    x <- residuals
    k <- n_regressors
  }

  n <- length(x)

  ((n-k+1)/6) * (skewness(x)^2 + (((kurtosis(x)-3)^2) / 4))
}
