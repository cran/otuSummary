calc_shannon <- function(data, base=exp(1)) {
  # Ensure the input is a numeric vector or matrix
  if (!is.numeric(data)) {
    stop("Input data must be a numeric vector or matrix.")
  } 
  # Calculate the p for each item
  total = sum(data)
  p = data/total
  # Calculate the Shannon index
  shannon_index = sum(-p * log(p,base = base), na.rm = TRUE)
  return(shannon_index)
}
