################ Phaethon Lifetime Dataframe Generator #########################
# Author : Richard Podkolinski
# Email  : devlar@gmail.com
# Purpose: Generates a dataframe with lifetimes from a Weibull mixture model. 
#          Capable of producing bathtub-like data with infant mortality
#           as well as old age failure with any censorship proportion.
################################################################################


generate_lifetimes = function(
  # Number of lifetimes
  n, 
  # Probability of censorship
  cen_prob = 0.1,
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
  seed=NULL,
  # Minimum and maximum number of abnormal days
  min_days = 15, max_days = 45,
  # Normal operating output
  mu_1 = 0, sd_1 = 1,
  # Abnormal operating output
  mu_2 = 0, sd_2 = 5
  ) {
  
  
  # Imports
  require(dplyr)
  source("stationary_to_failure.R", local = TRUE)
  source("rweibullmixcensored.R",   local = TRUE)
  
  # Set seed for reproducibility
  if(!is.null(seed)) {
    set.seed(seed = seed)
  } else {
    set.seed(seed = NULL )
  }

  # Generate censored Weibull mixture lifetimes
  df = rweibullmixcensored(n = n, 
                           cen_prob = cen_prob, 
                           inf_prob = inf_prob,
                           precision = precision, 
                           infant_shape = infant_shape, 
                           infant_scale = infant_scale, 
                           oldage_shape = oldage_shape, 
                           oldage_scale = oldage_scale, 
                           ceiling = ceiling)
  
  # Fill in stationary data until failure
  df = df %>%
    group_by(id) %>%
    do(
      left_join(
        data.frame(id      = .$id[1],
                   epoch   = seq(1, max(.$lifetime), 1),
                   energy  = stationary_to_failure( max(.$lifetime) )[1],
                   alarm   = stationary_to_failure( max(.$lifetime) )[2]
        ), #/data.frame
        ., by=c("id")) #/join
    ) #/do

  # Set the event boolean
  df = df %>% mutate(event = ifelse(epoch == lifetime, 1, 0))

  # Return truncated data
  if(complete == FALSE) {
    df = df %>%
      group_by(id) %>%
      slice(1:first(censor_time))
  }
  
  # return output
  return(df)
}