h_t = function(lifetime, nu, lambda){
  lambda * nu * lifetime^(nu - 1)
}

H_t = function(lifetime, nu, lambda){
  lambda * lifetime^nu
}

H_inv_t = function(lifetime, nu, lambda){
  ( (1 / lambda) * lifetime )^(1/nu)
}

Surv_Times = function(H_inv_t, N, ...){
  H_inv_t( (-log(runif(N))) , ...)
}


N = 5000
nu = 3
lambda = 5

real_times = Surv_Times(H_inv_t, N, nu = nu, lambda = lambda)
# plot(density(Surv_Times(H_inv_t, N, nu = 3, lambda = 10)))
plot(density(real_times))
cens_times = runif(N, min = 0, max = max(real_times))
d_i = as.numeric(real_times < cens_times)
min_times = apply(cbind(real_times, cens_times), 1, min)
obs_times = cbind(min_times, d_i)


stan_data = list(obs_times = obs_times, N = N)


library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit = stan(file = "Model_01/weibull_model.stan", data = stan_data, iter = 4000, chains = 4)

print(fit)

samps = fit@sim$samples[[1]]
# mean(samps$nu)
# mean(samps$lambda)

# med_res = function(t, nu){
#   (t^nu + log(2))^(1/nu)
# }

S_t = function(lifetime, nu, lambda){
  exp(-lambda * lifetime^nu)
}


 
df = data.frame(h_t = h_t(min_times, mean(samps$nu), mean(samps$lambda)),
                H_t = H_t(min_times, mean(samps$nu), mean(samps$lambda)),
                S_t0 = S_t(min_times, mean(samps$nu), mean(samps$lambda)),
                real_times = real_times,
                min_times = min_times,
                diff_times = real_times - min_times,
                mres = (min_times^mean(samps$nu) + log(2))^(1/mean(samps$nu)),
                d_i = d_i)

df = df[df$d_i == 0,]

plot(h_t ~ min_times, data=df)
plot(h_t ~ diff_times, data = df)


mean_squared_error = sum(df$diff_times) / N
print(mean_squared_error)

test_times = Surv_Times(H_inv_t, N, nu = nu, lambda = lambda)
