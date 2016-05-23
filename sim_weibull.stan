data {
  int<lower = 1> N;
  real<lower = 0> t[N];
  int x2[N];
}

parameters{
  real<lower = 0> beta2;
  real<lower = 0> shape;
  real<lower = 0> baseline;
  // real<lower = 0> mean_ttf;
}

model{
  vector[N] lambda;
  // shape ~ gamma(0.0001, 0.0001);
  // beta2 ~ gamma(0.0001, 0.0001);
  // baseline ~ gamma(0.0001, 0.0001);
  shape ~ normal(1, 10000);
  beta2    ~ normal(1, 10000);
  baseline ~ normal(1, 10000);
  for ( i in 1:N ) {
    lambda[i] <- baseline * exp( -(beta2 * x2[i]) );
  }
  t ~ weibull(shape, lambda );
}
generated quantities {
    
}
