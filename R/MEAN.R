MEAN <- function(data){
  if(is.null(dim(data))) out = mean(data)
  else out = rowMeans(data)
  return(out)
}
