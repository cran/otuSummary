calc_chao1 <- function(data) {
  # Ensure the input is a numeric vector or matrix
  if (!is.numeric(data)) {
    stop("Input data must be a numeric vector or matrix.")
  } 
  # Calculate the observed species (S_obs)
  Sobs <- sum(data>0) 
  # Calculate the counts of unique categories
  catalog <- table(data)  
  # Calculate the number of singletons 
  S <- sum(catalog== 1)
  # Calculate the number of doubletons
  D <- sum(catalog== 2)

  # Estimate the total species count using Chao1 formula
  chao1 <- round(Sobs + S*(S-1)/(2*(D+1)),3)
  return(chao1)
}
