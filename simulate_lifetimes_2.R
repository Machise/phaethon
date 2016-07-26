library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

N = 500


# Model 1
alp = 1.5
sig = 3

# Model 2
nu = alp
lambda = sig^(-alp)


# Data Generation
lifetimes = ((sig^alp) * -log(runif(N)))^(1/alp)
d_i = rep(1, N)

stan_data = list(
  lifetime = cbind(lifetimes,d_i),
  N = N)



m1 = "
functions {
  real log_h_t(real lifetime, real alp, real sig);
  real H_t(real lifetime, real alp, real sig);
  
  real log_h_t(real lifetime, real alp, real sig){
    return log( (alp/sig) ) + (alp - 1) * log( (lifetime / sig) );
  }
  
  real H_t(real lifetime, real alp, real sig){
    return ( (lifetime / sig) )^alp;
  }
  
  real surv_dens_log(vector cens_lifetimes, real alp, real sig){
    real lifetime;
    real d_i;
    
    lifetime <- cens_lifetimes[1];
    d_i      <- cens_lifetimes[2];
    
    return d_i * log_h_t(lifetime, alp, sig) - H_t(lifetime, alp, sig);
    
  }
}


data {
  int<lower=0> N;
  vector[2] lifetime[N];
}


parameters {
  real<lower=0> alp;
  real<lower=0> sig;
}


model {
  for(i in 1:N){
    lifetime[i] ~ surv_dens(alp, sig);
  }
  alp ~ lognormal(0, 1.5);
  sig ~ lognormal(0, 1.5);
}

"

m1f = stan(model_code = m1, data = stan_data, cores = 3, chains = 2, iter = 1e4, warmup = 1e3)

print(m1f)



m2 = "
functions {
  real log_h_t(real lifetime, real nu, real lambda);
  real H_t(real lifetime, real nu, real lambda);

  real log_h_t(real lifetime, real nu, real lambda){
    return log(lambda * nu) + (nu - 1) * log(lifetime);
  }

  real H_t(real lifetime, real nu, real lambda){
    return lambda * lifetime^nu;
  }

  real surv_dens_log(vector cens_lifetimes, real nu, real lambda){
    real lifetime;
    real d_i;
    
    lifetime <- cens_lifetimes[1];
    d_i      <- cens_lifetimes[2];
  
  return d_i * log_h_t(lifetime, nu, lambda) - H_t(lifetime, nu, lambda);
}

}


data {
  int<lower=0> N;
  vector[2] lifetime[N];
}


parameters {
  real<lower=0> nu;
  real<lower=0> lambda;
}

model {
  for(i in 1:N){
    lifetime[i] ~ surv_dens(nu, lambda);
  }
  nu ~ lognormal(0, 1.5);
  lambda ~ lognormal(0, 1.5);
}

"


m2f = stan(model_code = m2, data = stan_data, cores = 3, chains = 2, iter = 1e4, warmup = 1e3)

print(m2f)



### Now with covariate

beta_1   = 0.7
beta_2   = 0.3
# x_1      = rbinom(N, 1, 0.5)
x_1      = rpois(N, 6)
x_2      = rbinom(N, 1, 0.5)
lin_pred = x_1 * beta_1 + x_2 * beta_2
# Data Generation
lifetimes = ((sig^alp) * (-log(runif(N))* exp(-lin_pred)) )^(1/alp)
d_i = rep(1, N)

stan_data = list(
  lifetime = cbind(lifetimes,d_i),
  N = N,
  x_1 = x_1,
  x_2 = x_2)


m3 = "

functions {
  real log_h_t(real lifetime, real alp, real sig, real lin_pred);
  real H_t(real lifetime, real alp, real sig, real lin_pred);
  
  real log_h_t(real lifetime, real alp, real sig, real lin_pred){
    return log( (alp/sig) ) + (alp - 1) * log( (lifetime / sig) ) + lin_pred;
  }
  
  real H_t(real lifetime, real alp, real sig, real lin_pred){
    return exp(lin_pred) * ( (lifetime / sig) )^alp ;
  }
  
  real surv_dens_log(vector cens_lifetimes, real alp, real sig, real lin_pred){
    real lifetime;
    real d_i;
    
    lifetime <- cens_lifetimes[1];
    d_i      <- cens_lifetimes[2];
    
    return d_i * log_h_t(lifetime, alp, sig, lin_pred) - H_t(lifetime, alp, sig, lin_pred);
    
  }
}


data {
  int<lower=0> N;
  vector[2] lifetime[N];
  int x_1[N];
  int x_2[N];
}


parameters {
  real<lower=0> alp;
  real<lower=0> sig;
  real beta_1;
  real beta_2;
}


model {
  for(i in 1:N){
    lifetime[i] ~ surv_dens(alp, sig, beta_1 * x_1[i] + beta_2 * x_2[i]);
  }
  alp ~ lognormal(0, 1.5);
  sig ~ lognormal(0, 1.5);
  
  beta_1 ~ normal(0, 100);
  beta_2 ~ normal(0, 100);

}

"

m3f = stan(model_code = m3, data = stan_data, cores = 3, chains = 2, iter = 1e4, warmup = 1e3)

print(m3f)


# Now with more variables

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



N = 500

# Model 1
alp = 1.5
sig = 365

# Model 2
nu = alp
lambda = sig^(-alp)

# id	park	type	kWh_l30d	weather_l30d		err_code02_l30d	
beta_1      = 0.3 # err_code01_l30d
beta_2      = 3  # err_code02_l30d - Severe
beta_repair = -0.15 # repair_days

x_1      = rpois(N, 5) # err_code01_l30d
x_2      = rbinom(N, 1, 0.5) # err_code02_l30d - Severe
x_repair = rpois(N, 20) # repair_days

lin_pred = x_1 * beta_1 + x_2 * beta_2 + x_repair * beta_repair
# Data Generation
lifetimes = ((sig^alp) * (-log(runif(N))* exp(-lin_pred)) )^(1/alp)
lifetimes = ((sig^alp) * (-log(runif(N))* exp(-(x_repair * beta_repair))) )^(1/alp)
d_i = rep(1, N)

stan_data = list(
  lifetime = cbind(lifetimes,d_i),
  N = N,
  x_1 = x_1,
  x_2 = x_2,
  x_repair = x_repair)


m4 = "

functions {
  real log_h_t(real lifetime, real alp, real sig, real lin_pred);
  real H_t(real lifetime, real alp, real sig, real lin_pred);
  
  real log_h_t(real lifetime, real alp, real sig, real lin_pred){
    return log( (alp/sig) ) + (alp - 1) * log( (lifetime / sig) ) + lin_pred;
  }
  
  real H_t(real lifetime, real alp, real sig, real lin_pred){
    return exp(lin_pred) * ( (lifetime / sig) )^alp ;
  }
  
  real surv_dens_log(vector cens_lifetimes, real alp, real sig, real lin_pred){
    real lifetime;
    real d_i;
    
    lifetime <- cens_lifetimes[1];
    d_i      <- cens_lifetimes[2];
    
    return d_i * log_h_t(lifetime, alp, sig, lin_pred) - H_t(lifetime, alp, sig, lin_pred);
    
  }
}


data {
  int<lower=0> N;
  vector[2] lifetime[N];
  int x_1[N];
  int x_2[N];
  int x_repair[N];
}


parameters {
  real<lower=0> alp;
  real<lower=0> sig;
  real beta_1;
  real beta_2;
  real beta_repair;
}


model {
  for(i in 1:N){
    lifetime[i] ~ surv_dens(alp, sig, beta_1 * x_1[i] + beta_2 * x_2[i] + beta_repair * x_repair[i]);
  }
  alp ~ lognormal(0, 1.5);
  sig ~ lognormal(0, 1.5);
  
  beta_1 ~ normal(0, 100);
  beta_2 ~ normal(0, 100);
  beta_repair ~ normal(0, 100);

}

"

m4f = stan(model_code = m4, data = stan_data, cores = 3, chains = 2, iter = 1e4, warmup = 1e3)

print(m4f)
