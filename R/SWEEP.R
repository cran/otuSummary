SWEEP <- function(data){
  if(is.null(dim(data))) out = data/data
  else out = sweep(data, 2, colSums(data),"/")*100
  return(out)
}
