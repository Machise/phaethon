h_t = function(lifetime, nu, lambda, lin_pred){
  exp(lin_pred) * lambda * nu * lifetime^(nu - 1)
}

H_t = function(lifetime, nu, lambda, lin_pred){
  exp(lin_pred) * lambda * lifetime^nu
}

H_inv_t = function(lifetime, nu, lambda, lin_pred){
  ( (lifetime / (exp(lin_pred) * lambda)) )^(1/nu) 
}

Surv_Times = function(H_inv_t, N, ...){
  H_inv_t( (-log(runif(N))) , ...)
}


N = 500
nu = 3
lambda = 10

beta = 1

x_1 = c(rep(1, N/2), rep(0, N/2))
lin_pred = beta*x_1

real_times = Surv_Times(H_inv_t, N, nu = nu, lambda = lambda, lin_pred = lin_pred)
plot(density(real_times))
cens_times = runif(N, min = 0, max = max(real_times))
d_i = as.numeric(real_times < cens_times)
min_times = apply(cbind(real_times, cens_times), 1, min)
obs_times = cbind(min_times, d_i)


stan_data = list(obs_times = obs_times, N = N, x_1 = x_1)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit = stan(file = "Model_02/weibull_model_linpred.stan", data = stan_data, iter = 4000, chains = 4)

print(fit)
