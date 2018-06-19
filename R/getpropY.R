getpropY <- function(obj, choices){
  eigs <- eigenvals(obj)
  pop <- round(eigs/sum(eigs)*100, 2)
  laby <- paste0('(',pop[choices[2]],'%)')
  return(laby)
}
