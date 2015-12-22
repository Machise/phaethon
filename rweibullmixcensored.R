########## Phaethon Weibull Mixture Censored Lifetime Generator ################
# Author : Richard Podkolinski
# Email  : devlar@gmail.com
# Purpose: Produces n lifetimes from a 2-component Weibull mixture model 
#           with censoring.
#          Intended for the simulation of bathtub-esque data with incomplete obs
################################################################################


rweibullmixcensored = function(
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
  ceiling = TRUE
  ){
  # Import dependency
  source("rweibullmix.R", local = TRUE)
  
  # Generate lifetimes using rweibullmix
  lifetimes = rweibullmix(n = n, 
                          inf_prob = inf_prob, 
                          precision = precision,
                          infant_shape = infant_shape,
                          infant_scale = infant_scale,
                          oldage_shape = oldage_shape, 
                          oldage_scale = oldage_scale,
                          ceiling = ceiling)
  
  # Determine which values should be censored
  censored = rbinom( length(lifetimes), 1, cen_prob)
  
  # Output for loop, with censor time
  censored_time = numeric()
  
  for(i in 1:length(lifetimes)){
    
    if(censored[i] == 1){
      
      # Censor observation
      ## Generate a random number between 1 and t for the lifetime
      censor_time = runif(1,1,lifetimes[i])
      
      # Apply ceiling to lifetime
      if(ceiling == TRUE) {
        censor_time = ceiling(censor_time)
      }
      
    } else {
      # If not censored, keep original lifetime
      censor_time = lifetimes[i]
    }
    censored_time = c(censored_time, censor_time)
    # print( c( lifetimes[i], censored[i], censor_time ) )
    
  }
  
  # Construct dataframe output from vectors
  output = data.frame(id = 1:n, 
                      lifetime = lifetimes, 
                      censored = censored, 
                      censor_time = censored_time)
  
  return(output)
  
}