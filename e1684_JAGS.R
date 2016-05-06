library(survival)
library(runjags)
library(coda)
library(smcure)

data("e1684")

source("e1684_data.R")

cat("
model
  {
    for(i in 1:N)
      {
        //isCensored[i] ~ dinterval(t[i], t.cen[i])
        t[i] ~ dweibull(1,lambda[i])
        lambda[i] <- exp(beta0 + beta1 * trt[i])
      }  
    beta0 ~ dnorm(0, 1e-6)
    beta1 ~ dnorm(0, 1e-6)
    
    median0 <- log(2)/exp(beta0)
		median1 <- log(2)/exp(beta0+beta1)
    
    mean0 <- 1/exp(beta0)
    mean1 <- 1/exp(beta0+beta1)
    }
    ", file = "survival_cancer.jags"
)

cancer_fit = run.jags(data = lfe,
                      model = "survival_cancer.jags",
                      monitor = c("beta0", "beta1", "median0", "median1"),
                      sample = 1e4, burnin = 1e3, n.chains = 3)
print(cancer_fit)
plot(cancer_fit)
traceplot(as.mcmc(cancer_fit))
