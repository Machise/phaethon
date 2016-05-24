## Weibull Density Explore

#### Density Plot ####
t_obs = seq(0, 1000, by = 0.5)
f05 = dweibull(t_obs, 0.5, 300)
f10 = dweibull(t_obs, 1, 300)
f30 = dweibull(t_obs, 3, 300)


plot(t_obs, f30, type = "n")
lines(t_obs, f05, type ="l", col = "red")
lines(t_obs, f10, type ="l", col = "blue")
lines(t_obs, f30, type ="l", col = "magenta")

#### Probability Plot ####

p05 = pweibull(t_obs, 0.5, 300)
p10 = pweibull(t_obs, 1, 300)
p30 = pweibull(t_obs, 3, 300)

plot(t_obs, p30, type = "n")
lines(t_obs, p05, type ="l", col = "red")
lines(t_obs, p10, type ="l", col = "blue")
lines(t_obs, p30, type ="l", col = "magenta")

#### Quantile Plot ####

p_obs = seq(0,0.999, by = 0.01)

q05 = qweibull(p_obs, 0.5, 300)
q10 = qweibull(p_obs, 1, 300)
q30 = qweibull(p_obs, 3, 300)

plot(p_obs, q30, type = "n")
lines(p_obs, q05, type ="l", col = "red")
lines(p_obs, q10, type ="l", col = "blue")
lines(p_obs, q30, type ="l", col = "magenta")

#### Failure Rate ####
library(eha)

# Hazard
# res <- ifelse(x < 0, 0, shape * (x/scale)^(shape - 1)/scale)


h05 = hweibull(t_obs, 0.5, 300)
h10 = hweibull(t_obs, 1, 300)
h30 = hweibull(t_obs, 3, 300)

plot(t_obs, h30, type = "n")
lines(t_obs, h05, type ="l", col = "red")
lines(t_obs, h10, type ="l", col = "blue")
lines(t_obs, h30, type ="l", col = "magenta")


#### Properties of the Weibull ####

shape = 6
scale = 300
y = 1:450
loc = 0
f_x = (shape / scale) * ((y - loc) / scale)^(shape - 1) * exp(-( (y - loc) /scale)^shape)
# dweibull(y, shape, scale)
# Where f(y) >= 0,  y >= 0, shape & scale > 0, -Inf < loc < Inf

R_x = exp( -((y - loc)/ scale)^shape )
#1 - pweibull(1:5, shape, scale)

h_x = (shape/scale) * (y / scale)^(shape - 1)
# AKA shape * (x/scale)^(shape - 1)/scale
# hweibull(y, shape, scale)

H_x = (y/scale)^(shape)
#Hweibull(y, shape, scale)

# Time to Failure
mean_ttf = loc + scale * gamma( (1 / shape) + 1 ) 
median_ttf = loc + scale * (log(2))^(1/shape)
