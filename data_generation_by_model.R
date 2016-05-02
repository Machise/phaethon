## Data Generation Overview

library(rethinking)
library(dplyr)

## Model ##

# s_i ~ Binomial(n_i, p_i)
# logit(p_i) = \alpha_{POND[i]} 
# \alpha_{POND} ~ Normal(\alpha, \sigma)
# \alpha ~ Normal(0,1)
# \sigma ~ HalfCauchy(0,1)

## Set parameters
a   = 1.4
sig = 1.5
n   = 60
n_i = as.integer( rep( c(5,10, 25, 35), each = 15) ) 

## Set per pond intercept
a_pond = rnorm(n, mean = a, sd = sig)

## Compose a dataframe of values
dsim = data.frame(pond = 1:n, n_i = n_i, true_a = a_pond)

## Generate per pond survival probability
dsim$s_i = rbinom(n, prob = logistic(dsim$true_a), size = dsim$n_i)

## Calculate per pool probability
dsim$p_nopool = dsim$s_i / dsim$n_i


## Fit Model
m12_3 = map2stan(
  alist(
    s_i ~ dbinom(n_i, p),
    logit(p) <- a_pond[pond],
    a_pond[pond] ~ dnorm(a, sig),
    a ~ dnorm(0, 1),
    sig ~ dcauchy(0,1)
  ),
  data = dsim, iter = 1e4, warmup = 1000
)

## Retrieve Parameters
precis(m12_3, depth = 2)

## Insert alpha and probabilities into dataframe
dsim$a_est      = coef(m12_3)[1:60]
dsim$p_partpool = logistic(dsim$a_est)
dsim$p_true     = logistic(dsim$true_a)

## Insert error into dataframe
dsim$er_npool  = abs(dsim$p_nopool - dsim$p_true)
dsim$er_ppool  = abs(dsim$p_partpool - dsim$p_true)


## Plot Error
plot(1:60, dsim$er_npool, xlab = "Pond", ylab = "Absolute Error", col = rangi2, pch = 16)
points(1:60, dsim$er_ppool)



#### Now with Weibull Model ####


n = 1000
df = data.frame(
  inv = 1:n,
  lft = rweibull(n, 1.5, 365)
)
  

dens(df$lft)

dens(log(df$lft))


m14 = map2
