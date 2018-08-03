generate_data <- function(n, p) {
  covariates = matrix(rnorm(n*p),n)
  responses = rnorm(n,0,1)
  return (list(covariates,responses))
}
generate_data(3,4)