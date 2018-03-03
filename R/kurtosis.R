
kurtosis <- function(x){

  n <- length(x)

  (sum((x - mean(x))^4) / n) / ((sum((x - mean(x))^2) / n)^2)
}
