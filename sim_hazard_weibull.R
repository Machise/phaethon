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

w_scale = -(log(300 + log(10*x1 ) ))
x = rweibull(1e5, shape = 6, scale = exp(-w_scale) )
fit = survreg(Surv(x)~1, dist = "weibull")
summary(fit)
exp(fit$coefficients)
# So this work