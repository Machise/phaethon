library(rethinking)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source("C-Index.R")



# This is uncensored data currently
df = readRDS("Data/lifetime_dummy.rds")
# dfc = df[df$id <= 500,]
dfc = df
dfm = dfc[dfc$lifetime == dfc$epoch,]

dfm$alarm = dfm$alarm * 100

stan_data = list(
       id          = dfm$id,
       obs_time    = cbind(dfm$epoch, dfm$epoch+1, dfm$event),
       alarm       = dfm$alarm,
       energy      = dfm$energy,
       N           = max(dfm$id)
     ) 


fit = stan(file = "Model_06/rweibullfit.stan", data = stan_data, iter = 4000, chains = 4)

print(fit)

dfm
samps = extract(fit)

lin_preds = mean(samps$beta_energy) * dfm$energy + mean(samps$beta_alarm) * dfm$alarm

# start_time	end_time	censor	id	park	type	kWh_l30d	weather_l30d	err_code01_l30d	err_code02_l30d	repair_days
