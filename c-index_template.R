


eve = c(80,20,50,30)
non = c(30,20,50)


c_index = function(event, non_event){
  total_pairs = 0
  concordant  = 0
  ties        = 0
  for(i in event){
    for(j in non_event){
      message(paste(i, " ", j))
      if( i == j ){
        ties = ties + 1
      }
      if( i >  j ){
        concordant = concordant + 1
      }
      total_pairs = total_pairs + 1
    }
  }
  c_index = (concordant + (0.5) * ties) / total_pairs
  return(c(c_index = c_index, total_pairs = total_pairs, concordant = concordant, ties = ties))
}

c_index(eve, non)
