pchChoose <- function(mySite){
  if(is.null(mySite)) {
    pch = 16
  } else {
    l <- length(levels(mySite))
    if(l==1) pch = 16
    else if (l > 4) pch = 16
    else if (l>1 & l<5) pch = 15:18
  }
  return(pch)
}
