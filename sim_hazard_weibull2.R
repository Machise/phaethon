library(survival)
library(rstan)
x2 = rbinom(1e3, 1, p = 0.1)
beta2 = 1


bl_haz = 365
t_1 = bl_haz * rweibull(1e3, shape = 6, scale = exp(  -( (beta2 * x2) )  ) )
t_1 = bl_haz * rweibull(1e3, shape = 12, scale = exp(  -( (0.105) )  ) )

plot(density(t_1))

m1 = survreg(Surv(t_1) ~ x2, dist = "weibull")
summary(m1)
exp(m1$coefficients)


lfe = list(t = t_1, N = length(t_1), x2 = x2)

m2 = stan(file = "sim_weibull.stan", data=lfe, iter = 1e4, warmup = 1e3, chains = 1)
print(m2)
