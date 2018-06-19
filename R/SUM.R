SUM <- function(data){
  if(is.null(dim(data))) out = sum(data)
  else out = rowSums(data)
  return(out)
}
