library(rstan)

source("mice_data.R")

m1 = stan(file = "mice.stan", data=mice)
