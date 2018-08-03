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


run_simulation <- function(n_trials, n, p, cutoff){
  p_values = replicate(n_trials, {model_select(generate_data(n,p)[[1]], generate_data(n,p)[[2]], cutoff)})
  hist(unlist(p_values), main = "Histogram of p values")
}

n = c(100,1000, 10000)
p = c(10, 20, 50)
n_trials = 1000
cutoff = 0.05
expand.grid(n,p)
dim(expand.grid(n,p))

par(mfrow=c(3,3), mar=c(4,4,2,0.5)) # Setup grid, margins
for (j in 1:nrow(expand.grid(n,p))) {
  run_simulation(n_trials, n = expand.grid(n,p)[j,1], p = expand.grid(n,p)[j,2], cutoff = 0.05)
}