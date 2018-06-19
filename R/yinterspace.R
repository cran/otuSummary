yinterspace <- function(mySite){
  l <- length(levels(mySite))
  if (1<5) yinterspace = 1
  else if(l>=5 & l <11) yinterspace = 0.8
  else if(l>=11 & l <16) yinterspace = 0.7
  else {
    yinterspace = 0.6
    message('mySite factors have too much levels')
  }
  return(yinterspace)
}
