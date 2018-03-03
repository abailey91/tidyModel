durbin_watson <- function(x){

  x_lead <- dplyr::lead(x)

  x_lead[is.na(x_lead)] <- 0

  (sum((x-x_lead)^2)) / (sum((x^2)))
}
