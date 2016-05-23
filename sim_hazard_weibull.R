n = 10000
beta1 = 30; beta2 = -1
lambdaT = .002 # baseline hazard
lambdaC = .004  # hazard of censoring

x1 = rnorm(n,0)
x2 = rnorm(n,0)
# true event time
Time = rweibull(n, shape=1, scale=lambdaT*exp(-beta1*x1-beta2*x2)) 
Censor = rweibull(n, shape=1, scale=lambdaC)   #censoring time
time = pmin(Time,Censor)  #observed time is min of censored and true
event = time==Time   # set to 1 if event is observed

library(survival)
fit = survreg(Surv(time, event)~ x1 + x2, dist = "weibull")
summary(fit)
exp(fit$scale)


fit = survreg(Surv(t_obs, )~ x1 + x2, dist = "weibull", control = survreg.control(maxiter= 5000))
events = rep(1, length(t_obs))
weibreg(Surv(t_obs)~ 1)
class(c(t_obs, events, x1, x2))


x1 = rpois(1e5, 10) 
beta0 = 300
beta1 = 0.5
w_scale = (beta0) * (beta1 * x1 )
x = rweibull(1e5, shape = 6, scale = exp(-w_scale) )
fit = survreg(Surv(x)~x1, dist = "weibull")
summary(fit)
exp(fit$coefficients)


x2 = rbinom(1e5, 1, p = 0.5)
beta2 = 4

# bl_S = rnorm(1e5, 10, 2)
# bl_S = rweibull(1e5, 2, 10)
bl_S = rweibull(1e5, 6, 50)
# x = bl_S * rweibull(1e5, shape = 6, scale = exp(  -( (beta1 * x1) + (beta2 * x2) )  ) )
x = bl_S * rweibull(1e5, shape = 6, scale = exp(  -( (beta1 * x1) + (beta2 * x2) )  ) )

fit = survreg(Surv(x)~x1 + x2, dist = "weibull")
summary(fit)
exp(fit$coefficients)
t_hat = predict(fit)
log(t_hat)
T_i = exp(fit$coefficients[2:3]) * cbind(x1,x2)
T_i = rowSums(T_i)
T_i = T_i * bl_S



x1 = rpois(1e5, 10) 
# bl_S = rweibull(1e5, 6, 356)
bl_S = 365
x2 = rbinom(1e5, 1, p = 0.5)
beta2 = 0.1
  
# x = bl_S * rweibull(1e5, shape = 6, scale = exp(  -( (beta2 * x2) )  ) )
x = rweibull(1e5, shape = 6, scale = exp(  -( (beta2 * x2) )  ) )

fit = survreg(Surv(x)~x2, dist = "weibull")
summary(fit)
exp(fit$coefficients)

x_grid = seq(0,2,0.005)
f_x = dweibull(x_grid, shape = 6, scale = exp(  -( (beta2 * 0) )  ) ) 
S_x = pweibull(x_grid, shape = 6, scale = exp(  -( (beta2 * 0) )  ), lower.tail = F )
h_x = f_x / S_x
h_x[is.infinite(h_x)] = NA
H_x = cumsum(h_x)

plot(x_grid, f_x, type="l")
plot(x_grid, S_x, type="l")
plot(x_grid, h_x, type="l")
plot(x_grid, H_x, type="l")

qweibull(0.5, shape = 6, scale = exp(  -( (beta2 * 0) )  ), lower.tail = F)

w_scale = exp(  -( (beta2 * 0) )  )
h2_x = 6/w_scale * (x_grid / w_scale)^(6-1)
library(testthat)
expect_equal(h_x, h2_x)
