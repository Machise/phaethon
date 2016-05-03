library(rethinking)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

### From the beginning ####
n = 1000
df = data.frame(
  inv = 1:n,
  lft = rnorm(n, 45, 2)
)

mm1 = map2stan(
  alist(
    lft ~ dnorm(mu, sig),
    mu <- a,
    a ~ dnorm(0, 1),
    # sig ~ dgamma(0.01, 0.01)
    # sig ~ dexp(1)
    sig ~ dcauchy(0,1)
  ),
  data = df, 
  start = list(a = 0, sig = 2),
  chains = 3, iter = 1e4, warmup = 1e3
)

stancode(mm1)
precis(mm1)
pairs(mm1) # Should see randomness
plot(mm1)


#### Log-Normal Model ####

n = 1000



df = data.frame(
  inv = 1:n,
  lft = rnorm(n, log(45), 2)
)

dens(df$lft)

mm2 = map2stan(
  alist(
    lft ~ dnorm(mu, sig),
    log(mu) <- a,
    a ~ dnorm(0, 1),
    sig ~ dcauchy(0,1)
  ),
  data = df, 
  start = list(a = 0, sig = 2),
  chains = 3, iter = 1e4, warmup = 1e3
)

stancode(mm2)
precis(mm2)
pairs(mm2) # Should see randomness
plot(mm2)




