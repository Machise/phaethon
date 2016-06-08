library(survival)
library(runjags)
library(coda)
library(rstan)
library(rethinking)
source("e1684_data.R")


# Litmus Test
### Survival Package Fit
m1 = survreg(Surv(obs_t, event) ~ trt, data = dfe, dist = "weibull")
summary(m1)




## JAGS Model
cat("
    model
    {
    # Priors
    beta0 ~ dnorm(0, 1e-6)
    beta1 ~ dnorm(0, 1e-6)
    alpha ~ dgamma(0.001, 0.001)
    
    # Likelihood
    for(i in 1:N) {
      isCensored[i] ~ dinterval(t[i], t.cen[i])
      t[i] ~ dweibull(alpha,mu[i])
      mu[i] <- exp(beta0 + beta1 * trt[i])
      lambda[i] <- log(mu[i])
      }
    # Mean time to death
      
      #mu <- alpha * loggam(( 1 + (1 / lambda) ))

      median0 <- pow(log(2) * exp(-beta0), 1/alpha)
			median1 <- pow(log(2) * exp(-beta0-beta1), 1/alpha)


    # Predictions
    #for(t in 1:new_t) {
    #  S_t[t] <- exp( -(new_t[t] / alpha))^lambda 
    #  }
    }
    ", file = "survival_cancer_weibull.jags"
)

m2 = run.jags(data = lfe,
              model = "survival_cancer_weibull.jags",
              monitor = c("alpha", "beta0", "beta1", "median0", "median1"),
              sample = 1e5, burnin = 1e3, n.chains = 3)
print(m2)[,"Mean"]
summary(m2)

summary(m1)

m4 = stan(file = "e1684_STAN_weibull.stan", data = lfs, iter = 1e4, warmup = 1e3); precis(m4)
print(m4)
m4samp = m4@sim$samples

