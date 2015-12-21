################## Phaethon Stationary to Failure Generator ####################
# Author : Richard Podkolinski
# Email  : devlar@gmail.com
# Purpose: Produces a stationary data set with inflated variance at the tail
################################################################################

stationary_to_failure = function( 
  # Lifetime integer
  n, 
  # Minimum and maximum number of abnormal days
  min_days = 15, max_days = 45,
  # Normal operating output
  mu_1 = 0, sd_1 = 1,
  # Abnormal operating output
  mu_2 = 0, sd_2 = 5
  ) {
  
    # If n is lower than maximum number of abnormal days
    if (n >= max_days) {
      
      # Select a random number of days with abnormal values
      f_window = ceiling(runif(1, min_days, max_days))
      
      # Glue together normal operating values with abnormal on the end
      output = c(rnorm( (n-f_window) , mean = mu_1, sd = sd_1),
                 rnorm(    f_window,   mean = mu_2, sd = sd_2))
      
      return(output)
    } else {
      # Return random mix of normal and abnormal series
      
      f_window = ceiling(runif(1, 1, n))
      output = c(rnorm( (n-f_window) , mean = mu_1, sd = sd_1),
                 rnorm(    f_window,   mean = mu_2, sd = sd_2))
      return(output)
    } #/else
  
}