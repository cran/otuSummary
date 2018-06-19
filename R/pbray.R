pbray <- function(allComm, subComm, tolower = TRUE) {
  allComm <- as.matrix(allComm)
  subComm <- as.matrix(subComm)
  nr <- nrow(allComm)
  mat <- matrix(0, nrow = nr, ncol = nr)
  dimnames(mat) <- list(rownames(allComm),rownames(allComm))
  tmp <- vector(mode = "numeric", length = nr*(nr-1)/2)
  k = 1
  for (i in 1:(nr-1)){
    Pi <- allComm[i,]
    Ri <- subComm[i,]
    for (j in (i+1):nr) {
      denominator <- sum(Pi,allComm[j,])
      numerator <- sum(abs(Ri - subComm[j,]))
      tmp[k] <- numerator/denominator
      k = k+1
    }
    tmp
    rm(denominator, numerator)
  }
  mat[lower.tri(mat)] <- tmp
  rm(tmp,k)
  distmatrix <- as.dist(mat)
  if(!tolower) distmatrix <- as.matrix(distmatrix)
  invisible(distmatrix)
}
