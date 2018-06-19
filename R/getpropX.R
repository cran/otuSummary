getpropX <- function(obj, choices){
  eigs <- eigenvals(obj)
  pop <- round(eigs/sum(eigs)*100, 2)
  labx <- paste0("(", pop[choices[1]], "%)")
  return(labx)
}
