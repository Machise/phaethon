################ Phaethon Dummy Lifetime Generator #############################
# Author : Richard Podkolinski
# Email  : devlar@gmail.com
# Purpose: Allows a user to explore lifetime data through plotting
################################################################################

source("generate_lifetimes.R")



df = generate_lifetimes(  
  # Number of lifetimes to generate
  4000, 
  # Probability of censorship
  cen_prob = 0.4,
  # Probability of infant mortality
  inf_prob = 0.1,
  # Precision of lifetimes, default is days
  precision = 1,
  # Infant shape parameter
  infant_shape = 1.5,
  # Infant scale parameter in days
  infant_scale = 365,
  # Oldage shape parameter
  oldage_shape = 4.0,
  # Oldage scale parameter in days
  oldage_scale = 3650,
  # Round values up?
  ceiling = TRUE,
  # Complete or Censored Only Data?
  complete = TRUE,
  # Set seed for reproducibility? 
  seed=1234567,
  # Minimum and maximum number of abnormal days
  min_days = 15, max_days = 45,
  # Normal operating output
  mu_1 = 0, sd_1 = 1,
  # Abnormal operating output
  mu_2 = 0, sd_2 = 5)


save(df, file = "Data/lifetime_dummy.RData")

