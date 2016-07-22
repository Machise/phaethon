
# If you have censored and non-censored observations
c_index = function(df){
  
  # # Split censored and non-censored
  # dfe = df[df$d_i == 1,c("h_t", "obs_times")]
  # dfc = df[df$d_i == 0,c("h_t", "obs_times")]
  
  df = df[c("h_t", "obs_times", "d_i")]
  
  
  # Start with hazards.
  # given that hazard i > hazard j
  df = df[order(df$h_t, decreasing = TRUE),]
  # is observed time i > greater than observed time j ?
  # if so, concordant
  
  
  total_pairs = 0
  ties = 0
  concordant = 0
  
  for(i in 1:nrow(df)){
    
    
    # print(df[i,])
    print(i)
    
    h_i = df[i, "h_t"]
    
    for(j in 1:nrow(df)){
      
      h_j = df[j, "h_t"]
      
      if(h_i == h_j){
        ties = ties + 1
      }
      if(h_i > h_j){
        concordant = concordant + 1
      }
      print(c(i,j))
      # print(df[j,])
     
      
      total_pairs = total_pairs + 1 
    }
    
  }
  c_index = (concordant + (0.5) * ties) / total_pairs
  
  print(c(total_pairs = total_pairs))
  browser()
  
  
}
y = df[1:5,]

c_index(y)





##### Kang 2015 ####

# X := Survival Time
# Y := Predictive Measure

# Pr_c   = Pr(X_i  < X_j and Y_i  < Y_j OR X_i  > X_j AND Y_i  > Y_j)
# Pr_d   = Pr(X_i  < X_j and Y_i  > Y_j OR X_i  > X_j AND Y_i  < Y_j)
# Pr_tX  = Pr(X_i == X_j and Y_i  > Y_j OR X_i == X_j AND Y_i  < Y_j)
# Pr_tY  = Pr(X_i  < X_j and Y_i == Y_j OR X_i  > X_j AND Y_i == Y_j)
# Pr_tXY = Pr(X_i == X_j and Y_i == Y_j)



