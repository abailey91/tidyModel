relative_weights_importance <- function(lm_model = NULL, data = NULL,y_var = NULL){

  if(!is.null(lm_model)){

    x_vars <- colnames(lm_model$model)[-1]

    #run PCA on data to get orthogonal uncorrelated variables
    pca <- princomp(scale(lm_model$model[,-1]),cor = T)

    model_data <- lm_model$model

  }else{

    x_vars <- colnames(data)[!colnames(data) %in% y_var]

    model_data <- data %>%
      select(one_of(c(y_var,x_vars)))

    #run PCA on data to get orthogonal uncorrelated variables
    pca <- princomp(scale(model_data),cor = T)
  }

  #store the variables
  pca_vars <- pca$scores %>%
    as.data.frame()

  #save the loadings for the variables
  pca_loadings <- pca$loadings

  #bind the PCA variables to the original dataset
  model_data <- model_data %>%
    bind_cols(pca_vars)

  #regress the new orthogonal variables on the dependent

  form <- formula(glue::glue("scale({colnames(model_data)[1]}) ~ 0 + {paste(paste0('scale(',colnames(pca_vars),')'),collapse = '+')}"))

  #extract coefficients and caluclate squares
  pca_model_coefs <- lm(data = model_data,formula = form) %>%
    broom::tidy() %>%
    mutate(square_estimate = estimate^2) %>%
    filter(term != "(Intercept)")

  #calculate weight for each variable - loop through square loadings * square pca regression coefficients
  rel_weight <- lapply(1:nrow(pca_model_coefs),function(i){
    var_loadings <- pca_loadings[i,]^2

    sum(pca_model_coefs$square_estimate * var_loadings)
  }) %>%
    unlist() %>%
    as.character() %>%
    as.numeric()



  data.frame(Variable = x_vars,
             Score = rel_weight,
             Score_Normalised = rel_weight/sum(rel_weight),stringsAsFactors = F) %>%
    as_tibble() %>%
    arrange(desc(Score))
}
