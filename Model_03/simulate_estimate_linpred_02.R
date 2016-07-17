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


# N = 500
N = 5000
nu = 1.5
lambda = 3


# Inverter Type
## micro, string, central
## 0 = central
## 1 = micro
## 2 = string
type_size =
  c(125, 25, 0,
    75 , 50, 25,
    0  , 75, 25,
    40 ,  0, 10,
    0  , 30, 20)

type_size = type_size * 10

inv_type = character()
for(i in seq_along(type_size)){
  label = paste0(as.character(i %% 3))
  inv_type = c(inv_type, rep(label, type_size[i]))
}

inv_label = paste0("inv_type_",inv_type)

inv_type = model.matrix(~ 0 + inv_type)

betas = c(0.6, 0.0, 0.3)

lin_preds = inv_type %*% diag(betas)
lin_preds = rowSums(lin_preds)
names(lin_preds) = NULL


real_times = Surv_Times(H_inv_t, N, nu = nu, lambda = lambda, lin_pred = lin_preds)
# Add Noise
# real_times = rowSums(cbind(real_times, abs(rnorm(N,0, 0.001))))

plot(density(real_times))
cens_times = runif(N, min = 0, max = max(real_times))
d_i = as.numeric(real_times < cens_times)
min_times = apply(cbind(real_times, cens_times), 1, min)
obs_times = cbind(min_times, d_i)


stan_data = list(obs_times = obs_times, N = N, 
                 x_cen = inv_type[,1], x_str = inv_type[,3])



library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit = stan(file = "Model_03/weibull_model_linpred_3.stan", data = stan_data, iter = 4000, chains = 4)

print(fit)


samps = fit@sim$samples[[1]]

lin_preds = mean(samps$beta_cen) * inv_type[,1] + mean(samps$beta_str) * inv_type[,3]

df = data.frame(h_t = h_t(min_times, mean(samps$nu), mean(samps$lambda), lin_pred = lin_preds),
                H_t = H_t(min_times, mean(samps$nu), mean(samps$lambda), lin_pred = lin_preds),
                real_times = real_times,
                min_times = min_times,
                diff_times = real_times - min_times,
                inv_label = inv_label,
                mres = (min_times^mean(samps$nu) + log(2) * lin_preds)^(1/mean(samps$nu)),
                d_i = d_i)


df = df[df$d_i == 0,]

ggplot(df, aes(x = min_times, y = h_t, color = inv_label)) + geom_point()


ggplot(df, aes(x = diff_times, y = h_t, color = inv_label)) + geom_point()




# # Make Test Set, Test MSE on Test Data.
# test_times = Surv_Times(H_inv_t, N, nu = nu, lambda = lambda, lin_pred = lin_preds)
# cens_times = runif(N, min = 0, max = max(test_times))
# d_i = as.numeric(real_times < cens_times)
# min_times = apply(cbind(real_times, cens_times), 1, min)
# obs_times = cbind(min_times, d_i)

