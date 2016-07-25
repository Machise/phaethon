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
  real<lower=0> alp;
  real<lower=0> sig;
}

parameters {
  real<lower=0> nu;
  real<lower=0> lambda;
}

model {
  for(i in 1:N){
    lifetime[i] ~ surv_dens(alp, sig);
  }
  alp ~ lognormal(0, 1.5);
  sig ~ lognormal(0, 1.5);
}


model {
  for(i in 1:N){
    lifetime[i] ~ surv_dens(nu, lambda);
  }
  nu ~ lognormal(0, 1.5);
  lambda ~ lognormal(0, 1.5);
}




// MODEL 1

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


// MODEL 2


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


// Model 3 With Linear Predictors


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
}


parameters {
  real<lower=0> alp;
  real<lower=0> sig;
  real beta_1;
}


model {
  for(i in 1:N){
    lifetime[i] ~ surv_dens(alp, sig, beta_1 * x_1[i]);
  }
  alp ~ lognormal(0, 1.5);
  sig ~ lognormal(0, 1.5);
  
  beta_1 ~ normal(0, 100);

}