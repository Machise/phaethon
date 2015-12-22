############### Phaethon Weibull Mixture Lifetime Generator ####################
# Author : Richard Podkolinski
# Email  : devlar@gmail.com
# Purpose: Produces n lifetimes from a 2-component Weibull mixture model.
#           Intended for the simulation of bathtub-esque lifetime data
################################################################################


rweibullmix = function(
  # Number of lifetimes to generate
  n, 
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
  
  # Set the precision of the measures
  infant_scale = infant_scale * precision
  oldage_scale = oldage_scale * precision
  
  # Determine the number of infant deaths versus old age deaths
  inf_volm = sum( rbinom(n, 1, prob=inf_prob) )
  
  # Generate infant age at death
  infant = rweibull( inf_volm,       shape = infant_shape, scale = infant_scale )
  
  # Generate elderly age at death
  oldage = rweibull( (n - inf_volm), shape = oldage_shape, scale = oldage_scale )
  
  # Concat infant and oldage into a single output
  output = c(infant, oldage)
  output = output[order(output)]
  
  if(ceiling == TRUE) {
    output = ceiling(output)
    output = as.integer(output)
    tied_n = (n - length(unique(output)))
    message( paste("Tied Values :", tied_n ))
  }
  
  return(output)
}
