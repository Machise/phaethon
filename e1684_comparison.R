library(survival)
library(runjags)
library(coda)
library(rstan)
library(rethinking)
source("e1684_data.R")


# Litmus Test
### Survival Package Fit
m1 = survreg(Surv(obs_t, event) ~ trt, data = dfe, dist = "exponential")
summary(m1)


## JAGS Model
cat("
    model
    {
    for(i in 1:N)
    {
      isCensored[i] ~ dinterval(t[i], t.cen[i])
      t[i] ~ dweibull(1,lambda[i])
      lambda[i] <- exp(beta0 + beta1 * trt[i])
    }  
    beta0 ~ dnorm(0, 1e-6)
    beta1 ~ dnorm(0, 1e-6)
    
    }
    ", file = "survival_cancer.jags"
)

m2 = run.jags(data = lfe,
              model = "survival_cancer.jags",
              monitor = c("beta0", "beta1"),
              sample = 1e5, burnin = 1e3, n.chains = 3)
print(m2)[,"Mean"]








m3 = stan(file = "e1684_STAN.stan", data=lfs, iter = 1e4, warmup = 1e3); precis(m3)
# pairs(m3)
print(m3)
#m3 = stan(file = "mice.stan", data=lfs)

m4 = stan(file = "e1684_STAN_weibull.stan", data = lfs, iter = 1e4, warmup = 1e3); precis(m4)
