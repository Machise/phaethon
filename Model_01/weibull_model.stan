functions {
  real log_h_t(real lifetime, real nu, real lambda);
  real H_t(real lifetime, real nu, real lambda);
  
  real log_h_t(real lifetime, real nu, real lambda){
    return log(lambda * nu) + log(lifetime) * (nu - 1);
  }
  
  real H_t(real lifetime, real nu, real lambda){
    return lambda * lifetime^nu;
  }
  
  real surv_dens_log(vector cens_lifetimes, real nu, real lambda){
    real lifetime;
    real d_i;
    
    lifetime <- cens_lifetimes[1];
    d_i      <- cens_lifetimes[2];
    
    return(d_i * log_h_t(lifetime, nu, lambda)) - H_t(lifetime, nu, lambda);
  }
  

}

data {
  int<lower=0> N;
  vector[2] obs_times[N];
}

parameters {
  real<lower=0> nu;
  real<lower=0> lambda;
}

model {
  for(i in 1:N){
    obs_times[i] ~ surv_dens(nu, lambda);
  }
  
  // nu ~ lognormal(0,1000);
  // lambda ~ lognormal(0,1000);
  nu ~ gamma(1, 0.1);
  lambda ~ gamma(1, 0.1);
}