
pbray <- function(allComm, subComm, tolower = TRUE) {
  allComm <- as.matrix(allComm)
  subComm <- as.matrix(subComm)
  nr <- nrow(allComm)
  mat <- matrix(0, nrow = nr, ncol = nr)
  dimnames(mat) <- list(rownames(allComm),rownames(allComm))
  rsum = rowSums(allComm)
  denominator = as.dist(outer(rsum, rsum, FUN="+"))
  numerator = dist(subComm, method = 'minkowski', p =1) ## will cal |xi -yi|
  mat = numerator/denominator
  
  distmatrix <- as.dist(mat)
  if(!tolower) distmatrix <- as.matrix(distmatrix)
  invisible(distmatrix)
}

