colChoose <- function(mySite){
  if(is.null(mySite)) {
    colo = "steelblue"
  }else{
    l <- length(levels(mySite))
    if (l > 13) colo = "steelblue"
    else if(l < 3) colo = c("steelblue","tomato","gold")
    else colo = rainbow(n = l)
  }
  return(colo)
}
