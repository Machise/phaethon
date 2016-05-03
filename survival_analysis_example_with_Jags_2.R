# cancer <- read.table("http://goo.gl/3cnoam", header=TRUE)
summary(cancer)

censored = cancer$status == 0
is_censored = censored * 1
t_to_death = cancer$death
t_to_death[censored] = NA
t_to_death


t_cen = rep(0, times = length(censored))
t_cen[censored] = cancer$death[censored]
t_cen


# Put data together for JAGS
cancer_data = list(t_to_death = t_to_death,
                   t_cen = t_cen,
                   N = nrow(cancer),
                   group = rep(1:4, each = 30))

cat("
  model
    {
    # priors
    for (j in 1:4)
      {
      lambda[j] ~ dgamma(0.001, 0.001)
      mu[j] <- 1 / lambda[j]
      }
    #likelihood
    for (i in 1:N)
      {
        is_censored[i] ~ dinterval(t_to_death[i], t_cen[i])
        t_to_death[i] ~ dexp(lambda[group[i]])
      }
    }
", file = "survival_cancer.jags"
)

library(runjags)
library(coda)

cancer_fit = run.jags(data = cancer_data,
                      model = "survival_cancer.jags",
                      monitor = c("mu"),
                      sample = 1e4, burnin = 1e3, n.chains = 3)

par(mfrow = c(2,2))
densplot(as.mcmc(cancer_fit), xlim = c(2,20))
