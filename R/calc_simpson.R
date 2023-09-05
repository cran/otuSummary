calc_simpson <- function(data, index='simpson') {
  # Ensure the input is a numeric vector or matrix
  if (!is.numeric(data)) {
    stop("Input data must be a numeric vector or matrix.")
  } 
  # Calculate the p for each item
  total = sum(data)
  p = data/total
  H = sum(p^2, na.rm = TRUE)
  if (index == "simpson"){
    H = 1 - H
  } else if (index == "invsimpson") {
    H = 1/H
  }
  return(H)
}
