data{
  int<lower=0> N_uncensored;
  int<lower=0> N_censored;
  int<lower=0> trt_uncensored[N_uncensored];
  int<lower=0> trt_censored[N_censored];
  real<lower=0> t_censored[N_censored];
  real<lower=0> t_uncensored[N_uncensored];
}

parameters{
  real alpha;
  real beta0;
  real beta1;
  real<lower=1> t2_censored[N_censored]; // t_censored / t_censored
}
model{
  alpha ~ gamma(0.001, 0.001);
  beta1 ~ normal( 0 , 1e+06 );
  beta0 ~ normal( 0 , 1e+06 );
  for( i in 1:N_uncensored ) {
    // t_uncensored[i] ~ exponential( 1 / exp( -(beta0 + beta1 * trt_uncensored[i])) );
    t_uncensored[i] ~ weibull(alpha, exp( -(beta0 + beta1 * trt_uncensored[i])) );
  }
  for( i in 1:N_censored ){
    // increment_log_prob(weibull_ccdf_log(t_censored[i], 1, exp( -(beta0 + beta1 * trt_censored[i] )) / t_censored[i]));
    t2_censored[i] ~ weibull(alpha, exp( -(beta0 + beta1 * trt_censored[i] )) / t_censored[i]);
  }
}