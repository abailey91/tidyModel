


build_tidy_model_lm <- function(data,
                                y_var,
                                predictors,
                                standardise = FALSE,
                                ...){

  #set up grouping variables for nested data frame
  grouping_vars <- rlang::quos(...)

  #nest dataset
  data <- data %>%
    group_by(!!!grouping_vars) %>%
    nest() %>%
    mutate(model = purrr::map(data,function(dat){

      if(!standardise){
        form <- formula(glue::glue("{y_var}~{paste(predictors,collapse='+')}"))
      }else{
        form <- formula(glue::glue("{y_var}~{paste(paste0('scale(',predictors,')'),collapse='+')}"))
      }

      lm(data = dat,formula = form)

    })) %>%
    mutate(summary = map(model,broom::tidy)) %>% #coefficients and tstats
    mutate(Actual_vs_Fitted = pmap(list(model,data),function(mod,dat){ actual_vs_fitted(lm_model = mod,dates = dat$Date)})) %>%
    mutate(fit_RSQ = map_dbl(model,function(x){summary(x)$r.squared})) %>% #R Squared
    mutate(fit_AdjRSQ = map_dbl(model,function(x){summary(x)$adj.r.squared})) %>% # Adjusted R Squared
    mutate(fit_mse = map_dbl(model,mean_squared_error)) %>% # Mean Squared Error
    mutate(auto_DW = map_dbl(model,function(x){durbin_watson(x$residuals)})) %>% #Durbin Watson stat
    mutate(auto_BGTest = map_dbl(model,function(x){breusch_godfrey_lm_test(x, p = 2)})) %>% #2 period breusch godfrey
    mutate(normal_jarquebera = map_dbl(model,jarque_bera_test)) %>%  # Residual Jarque Bera
    mutate(var_importance = map(model,relative_weights_importance))
    #mutate(resid_skewness = map_dbl(model,function(x){skewness(x$residuals)})) %>% # Residual Skewness
    #mutate(resid_kurtosis = map_dbl(model,function(x){kurtosis(x$residuals)})) # Residual kurtosis
}
