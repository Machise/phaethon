functions {
  real log_h_t(real lifetime, real nu, real lambda, real lin_pred);
  real H_t(real lifetime, real nu, real lambda, real lin_pred);
  
  real log_h_t(real lifetime, real nu, real lambda, real lin_pred){
    return log(lambda * nu) + log(lifetime) * (nu - 1) + lin_pred;
  }
  
  real H_t(real lifetime, real nu, real lambda, real lin_pred){
    return exp(lin_pred) * lambda * lifetime^nu;
  }
  
  real surv_dens_log(vector cens_lifetimes, real nu, real lambda, real lin_pred){
    real lifetime;
    real d_i;
    
    lifetime <- cens_lifetimes[1];
    d_i      <- cens_lifetimes[2];
    
    return(d_i * log_h_t(lifetime, nu, lambda, lin_pred)) - H_t(lifetime, nu, lambda, lin_pred);
  }
  
  
}

data {
  int<lower=0> N;
  vector[2] obs_times[N];
  real x_cen[N];
  real x_str[N];
  
}

parameters {
  real<lower=0> nu;
  real<lower=0> lambda;
  real beta_cen;
  real beta_str;
}

model {
  for(i in 1:N){
    obs_times[i] ~ surv_dens(nu, lambda, beta_cen * x_cen[i] + beta_str * x_str[i]);
  }
  

  beta_cen ~ normal(0,1000);
  beta_str ~ normal(0,1000);
  nu ~ lognormal(0,1.5);
  lambda ~ lognormal(0,1.5);
}