# Deterministic SA
# Input for the method
d.SA.i <- function(var,percent){
  require(base)
  # Convert percent 
  for (i in percent){
    dec.percent = percent/100
  }
  # Test data frame for safety
  if (is.data.frame(var) == TRUE){
    var <- as.vector(unlist(var))
    new_var = var + dec.percent*var
  } else {
    new_var = var + dec.percent*var
  }
  return(new_var)
}

# Write to input file
i.w <- function(ite,var){
  write.table()
}
  