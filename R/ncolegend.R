ncolegend <- function(mySite){
  l <- length(levels(mySite))
  if (1<5) ncol = 1
  else if(l>=5 & l <11) ncol = 2
  else if(l>=11 & l <16) ncol = 3
  else {
    ncol = 3
    warning("mySite factors have too much levels")
  }
  return(ncol)
}
