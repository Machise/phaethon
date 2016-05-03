roaches <- read.table("http://goo.gl/NvtpNb", header=TRUE)
summary(roaches)

m1 = survreg(Surv(death, status) ~ weight, dist = "exponential", data = roaches)
summary(m1)


censored = roaches$status == 0
is_censored = censored * 1
t_to_death = roaches$death
t_to_death[censored] = NA
t_to_death

t_cen = rep(0, times = length(censored))
t_cen[censored] = roaches$death[censored]
t_cen

roach_data = list(
  t_to_death = t_to_death,
  t_cen = t_cen,
  N = nrow(roaches),
  weight = roaches$weight
)

cat("
    model
    {
      # priors
      beta0 ~ dnorm(0, 0.001)
      beta1 ~ dnorm(0, 0.001)

      # likelihood
      for(i in 1:N)
      {
        is_censored[i] ~ dinterval(t_to_death[i], t_cen[i])
        t_to_death[i] ~ dexp(lambda[i])

        lambda[i] <- 1/mu[i]
        log(mu[i]) <- beta0 + beta1*weight[i]
      }
   }
   ", file="survival_roaches.jags")


library(runjags)
library(coda)

roaches_fit = run.jags(data = roach_data,
                       model = "survival_roaches.jags",
                       monitor = c("beta0", "beta1"),
                       sample = 1e4, burnin = 5e3, n.chains = 3)

summary(roaches_fit)

par(mfrow = c(1,2))
densplot(as.mcmc(roaches_fit)[, "beta0"], xlim = c(1.5, 3.5))
abline(v=m1$coef[1], col="red")
densplot(as.mcmc(roaches_fit)[,"beta1"])
abline(v=m1$coef[2], col="red")
