deterministic.SA <- function(x,percent,sign){
  if (sign == 'pos'){
    x = x+x*percent
  } else {
    x = x-x*percent
  }
}