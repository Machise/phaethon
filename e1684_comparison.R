library(survival)
library(runjags)
library(coda)
source("e1684_data.R")

lfe$t
lfe$t.cen

event = ifelse(is.na(lfe$t), 0, 1)
obs_t = ifelse(is.na(lfe$t), lfe$t.cen, lfe$t)

Surv(obs_t, event)

dfe = data.frame(obs_t, event, trt = lfe$trt)

m1 = survreg(Surv(obs_t, event) ~ trt, data = lfe, dist = "exponential")
summary(m1)

lfe$isCensored = event = ifelse(is.na(lfe$t), 1, 0)

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
              sample = 1e4, burnin = 1e3, n.chains = 3)
print(m2)


library(rstan)

# Data needs to be reprocessed to separate 
censored = is.na(lfe$t)

N_uncensored = sum(!censored)
N_censored   = sum(censored)

t_uncensored = lfe$t[!censored]
t_censored   = lfe$t.cen[censored]

trt_uncensored = lfe$trt[!censored] + 1
trt_censored = lfe$trt[censored] + 1


# lfs = list(N_uncensored, N_censored, t_uncensored, t_censored, trt_uncensored, trt_censored, M=2)
lfs = list(N_uncensored = N_uncensored, 
           N_censored = N_censored, 
           t_uncensored = t_uncensored,
           censor_time = t_censored, 
           group_uncensored = trt_uncensored, 
           group_censored = trt_censored,
           M = 2)


lfs = list(N_uncensored = N_uncensored, 
           N_censored = N_censored, 
           t_uncensored = t_uncensored,
           t_censored = t_censored, 
           trt_uncensored = trt_uncensored, 
           trt_censored = trt_censored,
           M = 2)

m3 = stan(file = "e1684_STAN.stan", data=lfs)
m3 = stan(file = "mice.stan", data=lfs)
