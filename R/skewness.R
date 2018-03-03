skewness <- function(x){

  n <- length(x)

  (sum((x - mean(x))^3) / n) / ((sum((x - mean(x))^2) / n)^1.5)
}
