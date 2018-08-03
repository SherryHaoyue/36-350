generate_data <- function(n, p) {
  covariates = matrix(rnorm(n*p),n)
  responses = rnorm(n,0,1)
  return (list(covariates,responses))
}
generate_data(3,4)


model_select <- function(covariates, responses, cutoff) {
  result = lm(responses ~ covariates)
  new.covariates = covariates[,which(summary(result)$coefficients[-1,4] <= cutoff)]
  if (length(new.covariates)== 0) {return (vector(mode="numeric", length=0))}
  result2 = lm(responses ~ new.covariates)
  return (summary(result2)$coefficients[-1,4])
}