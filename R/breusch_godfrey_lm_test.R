breusch_godfrey_lm_test <- function(lm_model = NULL,p = 2, data = NULL, residuals = NULL){

  if(!is.null(lm_model)){
  model_data <- lm_model$model %>%
    as.data.frame()

  #get fitted model residuals
  resids <- lm_model$residuals

  }else{
    model_data <- data

    resids <- residuals
  }

  #remove scale from variable names if included in data - will be the case if the model has been standardised
  colnames(model_data)[-1] <- gsub("scale(",replacement = "",x = colnames(model_data)[-1],fixed = T)
  colnames(model_data)[-1] <- gsub(")",replacement = "",x = colnames(model_data)[-1],fixed = T)

  #create p lags of the errors - set NA to 0
  lagged_errors <- map(0:p,function(x){ dplyr::lead(resids,n = x) %>% as.data.frame() %>% rename(!!(glue::glue("resids_{x}")) := `.`)}) %>%
    bind_cols()# %>%
    #mutate_all(funs(replace(., is.na(.), 0)))

  #bind the errors to the regressor variables
  model_data <- model_data %>%
    bind_cols(lagged_errors) %>%
    drop_na()

  form <- formula(glue::glue("resids_0~{paste(colnames(model_data)[!colnames(model_data) %in% c('resids_0',colnames(model_data)[1])],collapse='+')}"))

  #fit the new model
  bg_model <- lm(data = model_data,formula = form)

  #get the approximate test statistic
  nrow(model_data) * summary(bg_model)$r.squared
}
