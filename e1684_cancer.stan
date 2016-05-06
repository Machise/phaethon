data {
  int<lower = 1> N;
  real<lower = 0> FAILTIME[N];
  int TRT[N];
}

parameters{
  real beta0;
  real beta1;
}

model{
    vector[N] lambda;
    beta1 ~ normal( 0 , 10000 );
    beta0 ~ normal( 0 , 10000 );
    for ( i in 1:N ) {
        lambda[i] <- exp(beta0 + beta1 * TRT[i]);
    }
    FAILTIME ~ weibull(1, lambda );
}