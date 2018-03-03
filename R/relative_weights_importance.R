relative_weights_importance <- function(lm_model){

  #run PCA on data to get orthogonal uncorrelated variables

  pca <- princomp(scale(lm_model$model[,-1]),cor = T)

  pca_vars <- pca$scores %>%
    as.data.frame()

  pca_loadings <- pca$loadings

  model_data <- lm_model$model %>%
    bind_cols(pca_vars)

  #regress the new orthogonal variables on the dependent

  form <- formula(glue::glue("scale({colnames(lm_model$model)[1]}) ~ 0 + {paste(paste0('scale(',colnames(pca_vars),')'),collapse = '+')}"))

  #extract coefficients and caluclate squares
  pca_model_coefs <- lm(data = model_data,formula = form) %>%
    broom::tidy() %>%
    mutate(square_estimate = estimate^2) %>%
    filter(term != "(Intercept)")

  #calculate weight for each variable - loop through square loadings * square pca regression coefficients
  rel_weight <- lapply(1:nrow(pca_model_coefs),function(i){
    var_loadings <- pca_loadings[,i]^2

    sum(pca_model_coefs$square_estimate * var_loadings)
  }) %>%
    unlist() %>%
    as.character() %>%
    as.numeric()



  data.frame(Variable = colnames(lm_model$model[,-1]),Score = rel_weight,stringsAsFactors = F)%>%
    as_tibble() %>%
    arrange(desc(Score))
}
