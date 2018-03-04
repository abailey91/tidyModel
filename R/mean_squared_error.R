mean_squared_error <- function(lm_model){

  fit <- fitted.values(lm_model)
  actual <- lm_model$model[,1]

  n <- length(fit)

  sum((actual - fit)^2) / n
}
