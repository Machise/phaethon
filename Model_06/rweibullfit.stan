functions {
  real log_h_t(real stop_time, real nu, real lambda, real lin_pred);
  real H_t(real stop_time, real nu, real lambda, real lin_pred);
  
  real log_h_t(real stop_time, real nu, real lambda, real lin_pred){
    return log(lambda * nu) + log(stop_time) * (nu - 1) + lin_pred;
  }
  
  real H_t(real stop_time, real nu, real lambda, real lin_pred){
    return exp(lin_pred) * lambda * stop_time^nu;
  }
  
  real surv_dens_log(vector cens_lifetimes, real nu, real lambda, real lin_pred){
    real start_time;
    real stop_time;
    real d_i;
    
    start_time <- cens_lifetimes[1];
    stop_time  <- cens_lifetimes[2];
    d_i        <- cens_lifetimes[3];

    
    return(d_i * log_h_t(stop_time, nu, lambda, lin_pred)) - H_t(stop_time, nu, lambda, lin_pred);
    //    return((d_i * (log(lambda * nu) + log(lifetime) * (nu - 1) + lin_pred)) - (exp(lin_pred) * lambda * lifetime^nu));
    
  }
  
  
}

data {
  int<lower=0> N;
  vector[3] obs_time[N];
  real energy[N];
  int<lower=0> alarm[N];
  
}

parameters {
  real<lower=0> nu;
  real<lower=0> lambda;
  real beta_energy;
  real beta_alarm;
}

model {
  for(i in 1:N){
    obs_time[i] ~ surv_dens(nu, lambda, beta_energy * energy[i] + beta_alarm * alarm[i]);
  }
  
  
  beta_energy ~ normal(0,1000);
  beta_alarm ~ normal(0,1000);
  nu ~ lognormal(0,1.5);
  lambda ~ lognormal(0,1.5);
}

/*generated quantities {
  vector[N] log_lik;
  for (n in 1:N){
    log_lik[n] <- surv_dens_log(obs_times[n], 
                                nu, lambda, beta_cen * x_cen[n] + beta_str * x_str[n]);
    
  }*/